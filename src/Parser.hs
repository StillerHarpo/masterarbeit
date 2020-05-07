{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language MultiWayIf #-}
{-# language LambdaCase #-}

module Parser where

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
type CtxP = [(Text, Expr)]

data ParserState = ParserState {
    _scLineFold :: Maybe (Parsec Void Text ())
  , _exprDefs:: Set Text
  , _inductiveDefs :: Set Text
  , _coinductiveDefs :: Set Text
  , _constructorDefs :: Set Text
  , _destructorDefs :: Set Text
  , _localVars :: [Text]
  }
$(makeLenses ''ParserState)

-- | a parser with a space consumer state for line folding
type Parser = StateT ParserState (Parsec Void Text)

instance (Monad m, IsString (m a)) => IsString (StateT s m a) where
  fromString = lift . fromString

parseTestS p t = parseTest (evalStateT p (ParserState Nothing Set.empty Set.empty Set.empty Set.empty Set.empty [])) t

parseProgram :: Parsec Void Text [Statement]
parseProgram = evalStateT (many parseStatement <* eof)
                          (ParserState Nothing Set.empty Set.empty Set.empty Set.empty Set.empty [])

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
  (\(name, _) -> do
     checkName name
     parseConstructorDef name)
  (\(name, gammaP) constructorDefsP -> do
     let (constructors, gamma1s, as, sigmas) = unzip4 constructorDefsP
         (_,gamma) = unzip gammaP
     localVars %= drop (length gamma)
     mapM_ checkName constructors
     inductiveDefs %= Set.insert name
     constructorDefs %= Set.union (Set.fromList constructors)
     pure InductiveDef{..})

parseCodata :: Parser Statement
parseCodata = nonIndented $ parseBlock
  (symbol "codata" *> parseDataHeader)
  (\(name, _) -> do
      checkName name
      parseDestructorDef name)
  (\(name, gammaP) destructorDefsP -> do
     let (destructors, gamma1s, sigmas, as) = unzip4 destructorDefsP
         (_,gamma) = unzip gammaP
     localVars %= drop (length gamma)
     mapM_ checkName destructors
     coinductiveDefs %= Set.insert name
     destructorDefs %= Set.union (Set.fromList destructors)
     pure CoinductiveDef{..})

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef name = parseStructorDef $ (,)
  <$ (inductiveDefs %= Set.insert name) -- Dummy
  <*> lexeme parseExpr
  <* (inductiveDefs %= Set.delete name) -- name is only allowed here to make it strictly positve
  <* symbol "->"
  <* lexeme (withPredicate (== name)
                           (`T.append` (" should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef name = parseStructorDef $ (,)
  <$ lexeme (withPredicate (== name)
                           (`T.append` ("should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <* (coinductiveDefs %= Set.insert name) -- Dummy
  <*> lexeme parseExpr
  <* (coinductiveDefs %= Set.delete name) -- name is only allowed here to make it strictly positve

parseDataHeader :: Parser (Text, CtxP)
parseDataHeader = do
  name <- lexeme parseTypeStrVarT
  void $ symbol ":"
  gammaP <- lexeme parseCtx
  let vars = map fst gammaP
  localVars %= (vars++)
  void $ if null gammaP
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  pure (name, gammaP)

parseStructorDef :: Parser (a,b) -> Parser (Text, Ctx,a,b)
parseStructorDef p = do
  nameC <- lexeme parseTypeStrVarT
  void $ symbol ":"
  gamma1P <- lexeme parseCtx
  let (vars,gamma1) = unzip gamma1P
  void $ if null gamma1
         then pure ""
         else symbol "->"
  (x,y) <- withLocalVars vars p
  pure (nameC, gamma1, x, y)

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

parseTerm :: Parser Expr
parseTerm = choice $ map lexeme
  [ parseUnitType
  , parseUnitExpr
  , try parseAbstr
  , try parseRec
  , try parseCorec
  , parseTypeStrVar
  , parseExprVar
  , parens parseExpr
  ]

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
  else case elemIndex var _localVars of
         Just idx -> pure $ LocalExprVar $ length _localVars - idx - 1
         Nothing  ->
           fancyFailure $ Set.singleton $ ErrorFail "Name not defined"

parseTypeStrVarT :: Parser Text
parseTypeStrVarT = withPredicate (`notElem` keywords)
                                 (`T.append` " is a keyword")
                                 (T.cons
                                  <$> upperChar
                                  <*> takeWhileP Nothing isAlphaNum)

parseTypeStrVar :: Parser Expr
parseTypeStrVar = do
  ParserState{..} <- get
  var <- parseTypeStrVarT
  if | var `Set.member` _inductiveDefs   -> pure $ Inductive var
     | var `Set.member` _coinductiveDefs -> pure $ Coinductive var
     | var `Set.member` _constructorDefs -> pure $ Constructor var
     | var `Set.member` _destructorDefs  -> pure $ Destructor var
     | otherwise                         -> pure $ TypeVar var

parseUnitType :: Parser Expr
parseUnitType = UnitType <$ string "Unit"

parseUnitExpr :: Parser Expr
parseUnitExpr = UnitExpr <$ string "()"

parseAbstr :: Parser Expr
parseAbstr = do
  (vars,ty1:tys) <- unzip <$> parseCtxNE
  void $ lexeme $ char '.'
  expr <- withLocalVars vars parseExpr
  pure $ foldr Abstr (Abstr ty1 expr) tys

parseRec :: Parser Expr
parseRec = parseBlock ((,)
                       <$ symbol "rec"
                       <*> lexeme parseTypeStrVarT
                       <* symbol "to"
                       <*> lexeme parseExpr
                       <* symbol "where")
                      (const parseMatch)
                      (((.).(.)) pure $ uncurry Rec)

parseCorec :: Parser Expr
parseCorec = parseBlock ((,)
                         <$ symbol "corec"
                         <*> lexeme parseExpr
                         <* symbol "to"
                         <*> lexeme parseTypeStrVarT
                         <* symbol "where")
                         (const parseMatch)
                         (((.).(.)) pure $ uncurry Corec)

parseMatch :: Parser Match
parseMatch = lineFold $ do
  structorName <- lexeme parseTypeStrVarT
  vars <- manyLexeme parseExprVarT
  void $ symbol "="
  matchExpr <- withLocalVars vars parseExpr
  pure Match{..}

-- | parses a non empty context
parseCtxNE :: Parser CtxP
parseCtxNE = symbol "(" *> parseCtxRest
  where parseCtxRest = do
          var <- lexeme parseExprVarT
          checkName var
          void $ symbol ":"
          expr <- parseExpr
          c <- char ',' <|> char ')'
          if c == ','
          then ((var,expr):) <$> withLocalVars [var] parseCtxRest
          else pure [(var,expr)]

parseCtx :: Parser CtxP
parseCtx = try parseCtxNE <|> pure []

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[ InfixL ((:@:) <$ symbol "@")]]

withLocalVars :: [Text] -> Parser a -> Parser a
withLocalVars vars p =
  (localVars %= (vars ++)) *> p <* (localVars %= drop (length vars))

-- | checks if name is already used.  We forbid name shadowing for now
checkName :: Text -- ^ name
          -> Parser ()
checkName name = do
  ParserState{..} <- get
  if name `Set.member` _exprDefs
    || name `Set.member` _inductiveDefs
    || name `Set.member` _coinductiveDefs
    || name `Set.member` _coinductiveDefs
    || name `Set.member` _constructorDefs
    || name `Set.member` _destructorDefs
  then fancyFailure $ Set.singleton $ ErrorFail "Name already defined"
  else pure ()

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
        pure $ L.IndentMany Nothing (f header) (pItems header)

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

