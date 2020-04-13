{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Parser where

import AbstractSyntaxTree

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as L

data ParserState = ParserState {
    scLineFold :: Parsec Void Text ()
  }

-- | a parser with a space consumer state for line folding
type Parser = StateT ParserState (Parsec Void Text)

data Variable = ExprVariable Text Expr
              | DataVariable Text [Text] Expr
  deriving (Eq, Show)

parseJudgment :: Parsec Void Text Judgment
parseJudgment = flip evalStateT (ParserState { scLineFold = sc }) $
  buildJudgment
  <$> many (try parseStatement)
  <*> nonIndented (lineFold parseExpr)
  <* eof

parseStatement :: Parser Variable
parseStatement = nonIndented $ choice
  [ parseData
  , parseCodata
  , parseDefinition
  ]

parseDefinition :: Parser Variable
parseDefinition = ExprVariable
  <$> lexeme parseExprVarT
  <* symbol "="
  <*> lineFold parseExpr

parseData :: Parser Variable
parseData = nonIndented $ parseBlock
  (symbol "data" *> parseDataHeader)
  (\(nameX, _, _) -> parseConstructorDef nameX)
  (\(nameX, typeParameters, gamma) constructors ->
      let (constrNames, gamma1s, as, sigmas) = unzip4 constructors
      in DataVariable nameX constrNames $ Inductive gamma sigmas as gamma1s)

parseCodata :: Parser Variable
parseCodata = nonIndented $ parseBlock
  (symbol "codata" *> parseDataHeader)
  (\(nameX, _, _) -> parseDestructorDef nameX)
  (\(nameX, typeParameters, gamma) destructors ->
      let (constrNames, gamma1s, sigmas, as) = unzip4 destructors
      in DataVariable nameX constrNames $ Coinductive gamma sigmas as gamma1s)

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <*> lexeme parseExpr
  <* symbol "->"
  <* lexeme (withPredicate (== nameX)
                           (`T.append` (" should be the same as type " `T.append` nameX))
                           parseTypeVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <* lexeme (withPredicate (== nameX)
                           (`T.append` ("should be the same as type " `T.append` nameX))
                           parseTypeVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <*> lexeme parseExpr

parseDataHeader :: Parser (Text, [Text], Ctx)
parseDataHeader = do
  nameX <- lexeme parseTypeVarT
  typeParameters <- many $ lexeme parseTypeVarT
  void $ symbol ":"
  gamma <- lexeme parseCtx
  void $ if null gamma
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  pure (nameX, typeParameters, gamma)

parseStructorDefHeader :: Parser (Text, Ctx)
parseStructorDefHeader = do
  nameC <- lexeme parseStructorVarT
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
  , parseTypeVar
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
  -- <*> many (lexeme parseTypeVar)

parseTypeVarT :: Parser Text
parseTypeVarT = withPredicate (`notElem` keywords)
                              (`T.append` " is a keyword")
                              (T.cons
                               <$> upperChar
                               <*> takeWhileP Nothing isAlphaNum)

parseStructorVarT :: Parser Text
parseStructorVarT = parseTypeVarT

parseTypeVar :: Parser Expr
parseTypeVar = TypeVar <$> parseTypeVarT

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
                      Rec

parseCorec :: Parser Expr
parseCorec = parseBlock (symbol "corec"
                         *> lexeme parseExpr
                         <* symbol "where")
                         (const parseMatch)
                         Corec

parseMatch :: Parser Match
parseMatch = Match
  <$> lexeme parseStructorVarT
  -- How does between type check?
  <*> lexeme (option [] . between (string "<") (string ">")
                        $ parseExpr `sepBy` symbol ",")
  <*> manyLexeme parseExprVarT
  <* symbol "="
  <*> lineFold parseExpr

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
parseBlock :: Parser a -> (a -> Parser b) -> (a -> [b] -> c) -> Parser c
parseBlock pHeader pItems f = try pBrackets <|> L.indentBlock (lift scn) pIndented
    where
      pBrackets = do
        header <- pHeader
        items <- between (symbol "{")
                         (symbol "}")
                         (lexeme (pItems header) `sepBy` symbol ";")
        pure $ f header items
      pIndented = do
        header <- pHeader
        pure $ L.IndentMany Nothing (pure . f header) (pItems header)

lineFold :: Parser a -> Parser a
lineFold p = lift . L.lineFold scn $
  \sc' -> evalStateT p (ParserState sc')

lexeme :: Parser a -> Parser a
lexeme p = do
  ParserState{..} <- get
  L.lexeme lineFolding p

manyLexeme :: Parser a -> Parser [a]
manyLexeme = lexeme . many . lexeme

-- TODO make it work for nested linefolds (maybe use a list of lineFolds)
lineFolding :: Parser ()
lineFolding = do
  ParserState{..} <- get
  try (lift scLineFold)
  <|> modify (\x -> x{scLineFold = sc}) *> lift sc <* newline

symbol :: Text -> Parser Text
symbol t = do
  ParserState{..} <- get
  L.symbol lineFolding t

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

buildJudgment :: [Variable] -> Expr -> Judgment
buildJudgment [] expr = Judgment Map.empty
                                 Map.empty
                                 Map.empty
                                 expr
buildJudgment (ExprVariable var exprV:vars) expr =
  case buildJudgment vars expr of
    Judgment ctx tyCtx strCtx expr
      -> Judgment (Map.insert var exprV ctx) tyCtx strCtx expr
buildJudgment (DataVariable tyVar strVars exprV:vars) expr =
  case buildJudgment vars expr of
    Judgment ctx tyCtx strCtx expr
      -> Judgment ctx
                  (Map.insert tyVar exprV tyCtx)
                  (foldr (`Map.insert` tyVar) strCtx strVars)
                  expr

unzip4 :: [(a,b,c,d)] -> ([a], [b], [c], [d])
unzip4 = foldl (\(as, bs, cs, ds) (a, b ,c ,d) -> (a:as,b:bs,c:cs,d:ds))
               ([],[],[],[])
