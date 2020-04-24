{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language MultiWayIf #-}

module Parser where

import AbstractSyntaxTree

import qualified Data.Set as Set
import Data.Set (Set)
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

data ParserState = ParserState {
    _scLineFold :: Parsec Void Text ()
  , _exprDefs:: Set Text
  , _inductiveDefs :: Set Text
  , _coinductiveDefs :: Set Text
  , _constructorDefs :: Set Text
  , _destructorDefs :: Set Text
  }
$(makeLenses ''ParserState)

-- | a parser with a space consumer state for line folding
type Parser = StateT ParserState (Parsec Void Text)

instance (Monad m, IsString (m a)) => IsString (StateT s m a) where
  fromString = lift . fromString

parseTestS p t = parseTest (evalStateT p (ParserState sc Set.empty Set.empty Set.empty Set.empty Set.empty)) t

parseProgram :: Parsec Void Text [Statement]
parseProgram = evalStateT (many parseStatement <* eof)
                          (ParserState sc Set.empty Set.empty Set.empty Set.empty Set.empty)

parseStatement :: Parser Statement
parseStatement = nonIndented $ choice
  [ try parseData
  , try parseCodata
  , try parseDefinition
  , Expression <$> lineFold parseExpr
  ]

parseDefinition :: Parser Statement
parseDefinition = do
  name <- lexeme parseExprVarT
  void $ symbol "="
  expr <- lineFold parseExpr
  checkName name
  exprDefs %= Set.insert name
  pure ExprDef{..}

parseData :: Parser Statement
parseData = nonIndented $ parseBlock
  (symbol "data" *> parseDataHeader)
  (\(name, _) -> do
     checkName name
     parseConstructorDef name)
  (\(name, gamma) constructorDefsP -> do
     let (constructors, gamma1s, as, sigmas) = unzip4 constructorDefsP
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
  (\(name, gamma) destructorDefsP -> do
     let (destructors, gamma1s, sigmas, as) = unzip4 destructorDefsP
     mapM_ checkName destructors
     coinductiveDefs %= Set.insert name
     destructorDefs %= Set.union (Set.fromList destructors)
     pure CoinductiveDef{..})

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef name = uncurry (,,,)
  <$> parseStructorDefHeader
  <* (inductiveDefs %= Set.insert name) -- Dummy
  <*> lexeme parseExpr
  <* (inductiveDefs %= Set.delete name) -- name is only allowed here to make it strictly positve
  <* symbol "->"
  <* lexeme (withPredicate (== name)
                           (`T.append` (" should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef name = uncurry (,,,)
  <$> parseStructorDefHeader
  <* lexeme (withPredicate (== name)
                           (`T.append` ("should be the same as type " `T.append` name))
                           parseTypeStrVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <* (coinductiveDefs %= Set.insert name) -- Dummy
  <*> lexeme parseExpr
  <* (coinductiveDefs %= Set.delete name) -- name is only allowed here to make it strictly positve

parseDataHeader :: Parser (Text, Ctx)
parseDataHeader = do
  name <- lexeme parseTypeStrVarT
  void $ symbol ":"
  gamma <- lexeme parseCtx
  void $ if null gamma
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  pure (name, gamma)

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
parseExprVar = do
  var <- lexeme parseExprVarT
  ParserState{..} <- get
  if var `Set.member` _exprDefs
  then pure $ GlobalExprVar var
  else pure $ LocalExprVar var

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

parseAbstr :: Parser (Expr -> Expr)
parseAbstr = (\((par1,ty1):pars) ->
                foldl (\abstrf (par,ty) -> abstrf . Abstr par ty)
                      (Abstr par1 ty1)
                      pars)
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
  <*> manyLexeme parseExprVarT
  <* symbol "="
  <*> lineFold parseExpr

parseCtxNE :: Parser Ctx
parseCtxNE = (:)
  <$ symbol "("
  <*> (do var <- lexeme parseExprVarT
          checkName var
          void $ symbol ":"
          (var,) <$> parseExpr )
  <*> many (do void $ symbol ","
               var <- lexeme parseExprVarT
               checkName var
               void $ symbol ":"
               (var,) <$> parseExpr )
  <* char ')'

parseCtx :: Parser Ctx
parseCtx = try parseCtxNE <|> pure []

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (try parseAbstr)
    ]
  , [ InfixL ((:@:) <$ symbol "@")
    ]
  ]

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
              <|> lift scn

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

