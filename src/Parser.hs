{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}

module Parser where

import Lib

import AbstractSyntaxTree

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (elemIndex)
import Data.String

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict
import Lens.Micro.Platform

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO reverse ctx

-- | Con/Destructor Context
-- maps Con/Destructor Names to their type names

-- | Context to parse
type CtxP = [(Text, TypeExpr)]

type TyCtxP = [(Text, CtxP)]

data InOrCoin = InTag | CoinTag

data ParserState = ParserState {
    _scLineFold :: Maybe (Parsec Void Text ())
  , _exprDefs:: Set Text
  , _typeExprDefs :: Set Text
  , _defDuctives :: [(Text, (InOrCoin, Ductive))]
  , _constructorDefs :: Set Text
  , _destructorDefs :: Set Text
  , _localExprVars :: [Text]
  , _localTypeVars :: [Text]
  }
$(makeLenses ''ParserState)

emptyState :: ParserState
emptyState = ParserState Nothing Set.empty Set.empty [] Set.empty Set.empty [] []

-- | a parser with a space consumer state for line folding
type Parser = StateT ParserState (Parsec Void Text)

instance (Monad m, IsString (m a)) => IsString (StateT s m a) where
  fromString = lift . fromString

parseProgram :: Parsec Void Text [Statement]
parseProgram = evalStateT (many parseStatement <* eof) emptyState

parseStatement :: Parser Statement
parseStatement = nonIndented $ choice
  [ try parseData
  , try parseCodata
  , try parseDefinition
  , Expression <$> lineFold parseExpr
  ]

parseDefinition :: Parser Statement
parseDefinition = lineFold $ do
  name <- lexeme parseExprVarT
  void $ symbol "="
  expr <- parseExpr
  checkName name
  exprDefs %= Set.insert name
  let ty = Nothing
  pure ExprDef{..}

parseData :: Parser Statement
parseData = nonIndented $ parseBlock
  (symbol "data" *> parseDataHeader)
  (\(name, _, _) -> do
     checkName name
     parseConstructorDef name)
  (\(name, gammaP, parametersCtxP) constructorDefsP -> do
     let nameDuc = name
         (strs, gamma1s, as, sigmas) = unzip4 constructorDefsP
         (_,gamma) = unzip gammaP
         parameterCtx = map (map snd . snd) parametersCtxP
     localExprVars .= []
     localTypeVars .= []
     mapM_ checkName strs
     let strNames = strs
     defDuctives %= ((name, (InTag, Ductive{..})):)
     typeExprDefs %= Set.insert name
     constructorDefs %= Set.union (Set.fromList strs)
     let typeExpr = In Ductive{..}
         kind = Nothing
     pure $ TypeDef{..})

parseCodata :: Parser Statement
parseCodata = nonIndented $ parseBlock
  (symbol "codata" *> parseDataHeader)
  (\(name, _, _) -> do
      checkName name
      parseDestructorDef name)
  (\(name, gammaP, parametersCtxP) destructorDefsP -> do
     let nameDuc = name
         (strs, gamma1s, sigmas, as) = unzip4 destructorDefsP
         gamma = map snd gammaP
         parameterCtx = map (map snd . snd) parametersCtxP
     localExprVars .= []
     localTypeVars .= []
     mapM_ checkName strs
     let strNames = strs
     defDuctives %= ((name, (CoinTag, Ductive{..})):)
     typeExprDefs %= Set.insert name
     destructorDefs %= Set.union (Set.fromList strs)
     let typeExpr = Coin Ductive{..}
         kind = Nothing
     pure TypeDef{..})

parseConstructorDef :: Text -> Parser (Text, Ctx, TypeExpr, [Expr])
parseConstructorDef name = parseStructorDef $ (,)
   -- name is only allowed here to make it strictly positve
  <$> withLocalTypeVar name (lexeme parseTypeExpr)
  <* symbol "->"
  <* lexeme (withPredicate (== name)
                           (`T.append` (" should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], TypeExpr)
parseDestructorDef name = parseStructorDef $ (,)
  <$ lexeme (withPredicate (== name)
                           (`T.append` ("should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
   -- name is only allowed here to make it strictly positve
  <*> withLocalTypeVar name (lexeme parseTypeExpr)

parseDataHeader :: Parser (Text, CtxP, TyCtxP)
parseDataHeader = do
  name <- lexeme parseTypeStrVarT
  parameters <- lexeme parseTyCtx
  localTypeVars .= map fst parameters
  void $ symbol ":"
  gammaP <- lexeme parseCtx
  void $ if null gammaP
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  pure (name, gammaP, parameters)

parseStructorDef :: Parser (a,b) -> Parser (Text, Ctx,a,b)
parseStructorDef p = do
  nameC <- lexeme parseTypeStrVarT
  void $ symbol ":"
  gamma1P <- lexeme parseCtx
  let (vars,gamma1) = unzip gamma1P
  void $ if null gamma1
         then pure ""
         else symbol "->"
  (x,y) <- withLocalExprVars vars p
  pure (nameC, gamma1, x, y)

parseTypeExpr :: Parser TypeExpr
parseTypeExpr = try (parseApp (:@) parseTypeTerm parseTerm) <|> parseTypeTerm

parseTypeTerm :: Parser TypeExpr
parseTypeTerm = choice $ map lexeme
  [ parseUnitType
  , try parseAbstr
  , parseTypeVar
  , parens parseTypeExpr
  ]

parseExpr :: Parser Expr
parseExpr = try (parseApp (:@:) parseTerm parseTerm) <|> parseTerm

parseTerm :: Parser Expr
parseTerm = choice $ map lexeme
  [ parseUnitExpr
  , try parseRec
  , try parseCorec
  , parseStrVar
  , parseExprVar
  , parens parseExpr
  ]

parseApp :: (a -> b -> a) -> Parser a -> Parser b -> Parser a
parseApp ap p1 p2 = do
  r <- ap <$> lexeme p1 <* symbol "@" <*> p2
  parseApp ap (pure r) p2 <|> pure r

keywords :: [Text]
keywords = ["data", "codata", "where", "rec", "corec"]

parseExprVarT :: Parser Text
parseExprVarT = withPredicate (`notElem` keywords)
                              (`T.append` " is a keyword")
                              (T.cons
                               <$> lowerChar
                               <*> takeWhileP Nothing isAlphaNum)

parseExprVar :: Parser Expr
parseExprVar = do
  var <- lexeme parseExprVarT
  ParserState{..} <- get
  if var `Set.member` _exprDefs
  then pure $ GlobalExprVar var
  else case elemIndex var _localExprVars of
         Just idx ->
           pure $ LocalExprVar (length _localExprVars - idx - 1)
                               var
         Nothing  ->
           fancyFailure $ Set.singleton $ ErrorFail "Name not defined"

parseTypeStrVarT :: Parser Text
parseTypeStrVarT = withPredicate (`notElem` keywords)
                                 (`T.append` " is a keyword")
                                 (T.cons
                                  <$> upperChar
                                  <*> takeWhileP Nothing isAlphaNum)

parseTypeVar :: Parser TypeExpr
parseTypeVar = do
  var <- parseTypeStrVarT
  ParserState{..} <- get
  if var `Set.member` _typeExprDefs
  then GlobalTypeVar var <$> parseParameters
  else case elemIndex var _localTypeVars of
         Just idx ->
           pure $ LocalTypeVar (length _localTypeVars - idx - 1)
                               var
         Nothing  ->
           fancyFailure $ Set.singleton $ ErrorFail "Name not defined"

parseStrVar :: Parser Expr
parseStrVar = do
  var <- parseTypeStrVarT
  parameters <- parseParameters
  let nameStr = var
      lookupStr [] = singleFailure "Con/Destrunctor not defined"
      lookupStr ((_,(inOrCoin,ductive@Ductive{..})):ductives) =
        case (saveIdx var strNames, inOrCoin) of
          (Just num, InTag) -> pure $ Constructor{..}
          (Just num, CoinTag) -> pure $ Destructor{..}
          (Nothing, _) -> lookupStr ductives
  withParameters parameters <$> ((view defDuctives <$> get) >>= lookupStr)

parseUnitType :: Parser TypeExpr
parseUnitType = UnitType <$ string "Unit"

parseUnitExpr :: Parser Expr
parseUnitExpr = UnitExpr <$ string "()"

parseAbstr :: Parser TypeExpr
parseAbstr = do
  (vars,ty1:tys) <- unzip <$> parseCtxNE
  void $ lexeme $ char '.'
  expr <- withLocalExprVars vars parseTypeExpr
  pure $ foldr Abstr (Abstr ty1 expr) tys

parseRec :: Parser Expr
parseRec = parseBlock ((,,)
                       <$ symbol "rec"
                       <*> lexeme parseParameters
                       <*> lexeme parseTypeStrVarT
                       <* symbol "to"
                       <*> lexeme parseTypeExpr
                       <* symbol "where")
                      (const parseMatch)
                      (\(parameters, from,toRec) matches -> do
                          (fromRec,matches) <- orderMatches from matches
                          pure (withParameters parameters Rec{..}))

parseCorec :: Parser Expr
parseCorec = parseBlock ((,,)
                         <$ symbol "corec"
                         <*> lexeme parseParameters
                         <*> lexeme parseTypeExpr
                         <* symbol "to"
                         <*> lexeme parseTypeStrVarT
                         <* symbol "where")
                         (const parseMatch)
                         (\(parameters, fromCorec,to) matches -> do
                             (toCorec,matches) <- orderMatches to matches
                             pure (withParameters parameters Corec{..}))

orderMatches :: Text -> [(Text,Expr)] -> Parser (Ductive,[Expr])
orderMatches name matches = do
  (_, ductive@Ductive{..}) <- (view defDuctives <$> get) >>= lookupP name
  exprs <- orderExprs strNames ([],matches)
  pure (ductive,exprs)
  where
    orderExprs [] ([],[]) = pure []
    orderExprs [] (m1,m2) = singleFailure $ "The following structors are not part of"
                                          <> name
                                          <> "or are duplicated \n"
                                          <> T.unwords (map fst (m1++m2))
    orderExprs (str:_) (_,[]) = singleFailure $ "Pattern matching non exhaustive for "
                                              <> str
    orderExprs s@(str:strs) (mf,m@(mStr,expr):ms)
      | str == mStr = (expr:) <$> orderExprs strs ([],mf ++ ms)
      | otherwise = orderExprs s (m:mf,ms)


parseMatch :: Parser (Text,Expr)
parseMatch = lineFold $ do
  structorName <- lexeme parseTypeStrVarT
  vars <- manyLexeme parseExprVarT
  void $ symbol "="
  matchExpr <- withLocalExprVars vars parseExpr
  pure (structorName,matchExpr)

-- | parses a non empty context
parseCtxNE :: Parser CtxP
parseCtxNE = symbol "(" *> parseCtxRest
  where parseCtxRest = do
          var <- lexeme parseExprVarT
          checkName var
          void $ symbol ":"
          expr <- parseTypeExpr
          c <- symbol "," <|> symbol ")"
          if c == ","
          then ((var,expr):) <$> withLocalExprVars [var] parseCtxRest
          else pure [(var,expr)]

parseCtx :: Parser CtxP
parseCtx = try parseCtxNE <|> pure []

parseTyCtxNE :: Parser TyCtxP
parseTyCtxNE = symbol "<" *> parseTyCtxRest
  where parseTyCtxRest = do
          var <- lexeme parseTypeStrVarT
          checkName var
          void $ symbol ":"
          ctx <- parseCtx
          void $ if null ctx
                 then symbols ["Set"]
                 else symbols ["->", "Set"]
          c <- symbol "," <|> symbol ">"
          if c == ","
          then ((var,ctx):) <$> withLocalTypeVar var parseTyCtxRest
          else pure [(var,ctx)]

parseTyCtx :: Parser TyCtxP
parseTyCtx = try parseTyCtxNE <|> pure []

parseParametersNE :: Parser [TypeExpr]
parseParametersNE = symbol "<" *> parseParametersRest
  where parseParametersRest = do
          tyExpr <- lexeme parseTypeExpr
          c <- symbol "," <|> symbol ">"
          if c == ","
          then (tyExpr:) <$> parseParametersRest
          else pure [tyExpr]

parseParameters :: Parser [TypeExpr]
parseParameters = try parseParametersNE <|> pure []

withParameters :: [TypeExpr] -> Expr -> Expr
withParameters [] = id
withParameters ps = WithParameters ps

withLocalExprVars :: [Text] -> Parser a -> Parser a
withLocalExprVars vars p =
  (localExprVars %= (++ vars))
  *> p
  <* (localExprVars %= \allVars -> take (length allVars - length vars) allVars)

withLocalTypeVar :: Text -> Parser a -> Parser a
withLocalTypeVar var p =
  (localTypeVars %= (++ [var]))
  *> p
  <* (localTypeVars %= \allVars -> take (length allVars - 1) allVars)

-- | checks if name is already used.  We forbid name shadowing for now
checkName :: Text -- ^ name
          -> Parser ()
checkName name = do
  ParserState{..} <- get
  if name `Set.member` _exprDefs
    || name `Set.member` _typeExprDefs
    || name `Set.member` _constructorDefs
    || name `Set.member` _destructorDefs
  then fancyFailure $ Set.singleton $ ErrorFail "Name already defined"
  else pure ()

-- | lookup lifted to the parser monad
lookupP :: Text -> [(Text,b)] -> Parser b
lookupP var ctx = case lookup var ctx of
                     Just t   -> pure t
                     Nothing -> singleFailure $ "Variable not defined: " <> var

-- | The space consumer with newline
scn :: Parsec Void Text ()
scn = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

-- | The space consumer without newline
sc :: Parsec Void Text ()
sc = L.space
  (void $ some $ char '\t' <|> char ' ')
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

nonIndented :: Parser a -> Parser a
nonIndented p = symbol ";" *> p <|> L.nonIndented (lift scn) p

parseBlock :: Parser a -> (a -> Parser b) -> (a -> [b] -> Parser c) -> Parser c
parseBlock pHeader pItems f =
  try pBrackets
  <|> noLineFold (L.indentBlock (lift scn) pIndented)
    where
      pBrackets = do
        header <- pHeader
        items <- between (symbol "{")
                         (symbol "}")
                         (lexeme (pItems header) `sepBy` symbol ";")
        f header items
      pIndented = do
        header <- pHeader
        pure $ L.IndentMany Nothing (f header . reverse) (pItems header)

lineFold :: Parser a -> Parser a
lineFold p = do
  parserState <- get
  (a, innerState) <- lift $ L.lineFold scn $
      \sc' -> runStateT (p <* lift scn) (set scLineFold (Just sc') parserState)
  put $ set scLineFold Nothing innerState
  pure a

noLineFold :: Parser a -> Parser a
noLineFold p = do
  lf <- view scLineFold <$> get
  scLineFold .= Nothing
  a <- p
  scLineFold .= lf
  pure a

lexeme :: Parser a -> Parser a
lexeme p = view scLineFold <$> get >>= \case
                  Just sc' -> try (L.lexeme (lift sc') p) <|> p
                  Nothing -> L.lexeme (lift sc) p

manyLexeme :: Parser a -> Parser [a]
manyLexeme = lexeme . many . lexeme

symbol :: Text -> Parser Text
symbol t = view scLineFold <$> get >>= \case
                  Just sc' -> try (L.symbol (lift sc') t) <|> string t
                  Nothing -> L.symbol (lift sc) t


symbols :: [Text] -> Parser [Text]
symbols = traverse symbol

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

withPredicate
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> (a -> Text)       -- ^ Message to print when the check fails)
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else parseError (FancyError o (Set.singleton (ErrorFail . T.unpack $ msg r)))

singleFailure :: Text -> Parser a
singleFailure = fancyFailure . Set.singleton . ErrorFail . T.unpack

unzip4 :: [(a,b,c,d)] -> ([a], [b], [c], [d])
unzip4 = foldl (\(as, bs, cs, ds) (a, b ,c ,d) -> (a:as,b:bs,c:cs,d:ds))
               ([],[],[],[])

