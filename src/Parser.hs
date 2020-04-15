{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}

module Parser where

import AbstractSyntaxTree

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
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

-- | Con/Destructor Context
-- maps Con/Destructor Names to their type names
type StrCtx = Map Text Text

data ParserState = ParserState {
    _scLineFold :: Parsec Void Text ()
  , _ctx :: Ctx
  , _tyCtx :: TyCtx
  , _strCtx :: StrCtx
  }
$(makeLenses ''ParserState)

-- | a parser with a space consumer state for line folding
type Parser = StateT ParserState (Parsec Void Text)

instance (Monad m, IsString (m a)) => IsString (StateT s m a) where
  fromString = lift . fromString

parseJudgment :: Parsec Void Text Judgment
parseJudgment = flip evalStateT (ParserState { _scLineFold = sc
                                             , _ctx = Map.empty
                                             , _tyCtx = Map.empty
                                             , _strCtx = Map.empty }) $ do
  void $ many $ try parseStatement
  ParserState{..} <- get
  expr <- nonIndented (lineFold parseExpr)
  pure $ Judgment _ctx _tyCtx expr

parseStatement :: Parser ()
parseStatement = nonIndented $ choice
  [ parseData
  , parseCodata
  , parseDefinition
  ]

parseDefinition :: Parser ()
parseDefinition = do
  ParserState{..} <- get
  name <- lexeme parseExprVarT
  void $ symbol "="
  expr <- lineFold parseExpr
  if Map.member name _ctx
  then fancyFailure $ Set.singleton $ ErrorFail "Name already defined"
  else ctx %= Map.insert name expr

parseData :: Parser ()
parseData = nonIndented $ parseBlock
  (symbol "data" *> parseDataHeader)
  (\(nameX, _, _) -> parseConstructorDef nameX)
  (\(nameX, typeParameters, gamma) constructors -> do
      let (constrNames, gamma1s, as, sigmas) = unzip4 constructors
      ParserState{..} <- get
      if nameX `Map.member` _ctx || any (`Map.member` _strCtx) constrNames
      then fancyFailure $ Set.singleton $ ErrorFail "Name already defined"
      else (do tyCtx %= Map.insert nameX (Inductive gamma sigmas as gamma1s)
               strCtx .= foldr (`Map.insert` nameX) _strCtx constrNames))

parseCodata :: Parser ()
parseCodata = nonIndented $ parseBlock
  (symbol "codata" *> parseDataHeader)
  (\(nameX, _, _) -> parseDestructorDef nameX)
  (\(nameX, typeParameters, gamma) destructors -> do
      let (destrNames, gamma1s, sigmas, as) = unzip4 destructors
      ParserState{..} <- get
      if nameX `Map.member` _ctx || any (`Map.member` _strCtx) destrNames
      then fancyFailure $ Set.singleton $ ErrorFail "Name already defined"
      else (do tyCtx %= Map.insert nameX (Coinductive gamma sigmas as gamma1s)
               strCtx .= foldr (`Map.insert` nameX) _strCtx destrNames))

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <* (tyCtx %= Map.insert nameX UnitType) -- Dummy
  <*> lexeme parseExpr
  <* (tyCtx %= Map.delete nameX) -- nameX is only allowed here to make it strctly positve
  <* symbol "->"
  <* lexeme (withPredicate (== nameX)
                           (`T.append` (" should be the same as type " `T.append` nameX))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <* lexeme (withPredicate (== nameX)
                           (`T.append` ("should be the same as type " `T.append` nameX))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <* (tyCtx %= Map.insert nameX UnitType) -- Dummy
  <*> lexeme parseExpr
  <* (tyCtx %= Map.delete nameX) -- nameX is only allowed here to make it strictly positve

parseDataHeader :: Parser (Text, [Text], Ctx)
parseDataHeader = do
  nameX <- lexeme parseTypeStrVarT
  typeParameters <- lexeme . option []
                           $ between "<" ">" (parseTypeStrVarT `sepBy1` ",")
  void $ symbol ":"
  gamma <- lexeme parseCtx
  void $ if null gamma
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  pure (nameX, typeParameters, gamma)

parseStructorDefHeader :: Parser (Text, Ctx)
parseStructorDefHeader = do
  nameC <- lexeme parseTypeStrVarT
  void $ symbol ":"
  gamma1 <- lexeme parseCtx
  void $ if null gamma1
         then pure ""
         else symbol "->"
  pure (nameC, gamma1)

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operatorTable

parseTerm :: Parser Expr
parseTerm = choice $ map lexeme
  [ parseUnitType
  , parseUnitExpr
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
parseExprVar = ExprVar
  <$> lexeme parseExprVarT

parseTypeStrVarT :: Parser Text
parseTypeStrVarT = withPredicate (`notElem` keywords)
                                 (`T.append` " is a keyword")
                                 (T.cons
                                  <$> upperChar
                                  <*> takeWhileP Nothing isAlphaNum)

parseTypeStrVar :: Parser Expr
parseTypeStrVar = do
  ParserState{..} <- get
  tyStrVar <- parseTypeStrVarT
  indexes <- parseIndexes
  let lookupStr = do
        nameX <- Map.lookup tyStrVar _strCtx
        (nameX,) <$> Map.lookup nameX _tyCtx
  if tyStrVar `Map.member` _tyCtx
  then pure $ TypeVar tyStrVar indexes
  else (case lookupStr of
          Just (nameX, Inductive{..})
                  -> pure $ Constructor tyStrVar indexes nameX
          Just (nameX, Coinductive{..})
                  -> pure $ Destructor tyStrVar indexes nameX
          Nothing -> singleFailure "Name not defined")

parseUnitType :: Parser Expr
parseUnitType = UnitType <$ string "Unit"

parseUnitExpr :: Parser Expr
parseUnitExpr = UnitExpr <$ string "()"

parseAbstr :: Parser (Expr -> Expr)
parseAbstr = (\((par1,ty1):pars) ->
                foldl (\abstrf (par,ty) -> abstrf . Abstr par ty)
                      (Abstr par1 ty1)
                      pars) . Map.toList
  <$> parseCtxNE
  <* char '.'

parseRec :: Parser Expr
parseRec = parseBlock (symbol "rec"
                       *> lexeme parseExpr
                       <* symbol "where")
                      (const parseMatch)
                      (((.).(.)) pure  Rec)

parseCorec :: Parser Expr
parseCorec = parseBlock (symbol "corec"
                         *> lexeme parseExpr
                         <* symbol "where")
                         (const parseMatch)
                         (((.).(.)) pure Corec)

parseMatch :: Parser Match
parseMatch = Match
  <$> lexeme parseTypeStrVarT
  <*> lexeme parseIndexes
  <*> manyLexeme parseExprVarT
  <* symbol "="
  <*> lineFold parseExpr

parseIndexes :: Parser [Expr]
parseIndexes = option [] . between "<" ">"
                         $ parseExpr `sepBy` symbol ","

parseCtxNE :: Parser Ctx
parseCtxNE = ((.).(.)) Map.fromList (:)
  <$ symbol "("
  <*> ((,)
       <$> lexeme parseExprVarT
       <* symbol ":"
       <*> parseExpr)
  <*> many ((,)
            <$ symbol ","
            <*> lexeme parseExprVarT
            <* symbol ":"
            <*> parseExpr)
  <* char ')'

parseCtx :: Parser Ctx
parseCtx = try parseCtxNE <|> pure Map.empty

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (try parseAbstr)
    ]
  , [ InfixL ((:@:) <$ symbol "@")
    ]
  ]

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
parseBlock pHeader pItems f = try pBrackets <|> L.indentBlock (lift scn) pIndented
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
  parserState@ParserState {..} <- get
  (a, innerState) <- lift $ L.lineFold scn $
      \sc' -> runStateT p (set scLineFold sc' parserState)
  put $ set scLineFold _scLineFold innerState
  pure a

lexeme :: Parser a -> Parser a
lexeme = L.lexeme lineFolding

manyLexeme :: Parser a -> Parser [a]
manyLexeme = lexeme . many . lexeme

lineFolding :: Parser ()
lineFolding = (view scLineFold <$> get >>= try . lift)
            <|> lift sc <* newline
            <|> lift scn <* eof

symbol :: Text -> Parser Text
symbol = L.symbol lineFolding

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

