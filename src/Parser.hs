{-# language OverloadedStrings #-}

module Parser where

import AbstractSyntaxTree

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (newline)
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Variable = ExprVariable Text Expr
              | DataVariable Text [Text] Expr
  deriving (Eq, Show)

parseJudgment :: Parser Judgment
parseJudgment = buildJudgment
  <$> many (try $ lexeme $ parseStatement <* newline)
  <*> parseExpr

parseStatement :: Parser Variable
parseStatement = choice
  [ parseData
  , parseCodata
  , parseDefinition
  ]

parseDefinition :: Parser Variable
parseDefinition = ExprVariable
  <$> lexeme parseExprVarT
  <* symbol "="
  <*> parseExpr

parseData :: Parser Variable
parseData = do
  void $ symbol "data"
  (nameX, typeParameters, gamma) <- parseDataHeader
  constructors <- lexeme $ parseConstructorDef nameX
                           `sepBy`
                           newline
  let (constrNames, gamma1s, as, sigmas) = unzip4 constructors
  pure $ DataVariable nameX constrNames $ Inductive gamma sigmas as gamma1s

parseCodata :: Parser Variable
parseCodata = do
  void $ symbol "codata"
  (nameX, typeParameters, gamma) <- parseDataHeader
  destructors <- lexeme $ parseDestructorDef nameX
                          `sepBy`
                          newline
  let (constrNames, gamma1s, sigmas, as) = unzip4 destructors
  pure $ DataVariable nameX constrNames $ Coinductive gamma sigmas as gamma1s

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <*> lexeme parseExpr
  <* symbol "->"
  <* lexeme (withPredicate (== nameX)
                           ("Should be the same as type " `T.append` nameX)
                           parseTypeVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef nameX = uncurry (,,,)
  <$> parseStructorDefHeader
  <* lexeme (withPredicate (== nameX)
                           ("Should be the same as type " `T.append` nameX)
                           parseTypeVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <*> lexeme parseExpr

parseDataHeader :: Parser (Text, [Text], Ctx)
parseDataHeader = do
  nameX <- lexeme parseTypeVarT
  typeParameters <- many $ lexeme parseTypeVarT
  void $ symbol ":"
  gamma <- parseCtx
  void $ if null gamma
         then symbols ["Set", "where"]
         else symbols ["->", "Set", "where"]
  void $ lexeme newline
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
parseTerm = choice
  [ parseUnitType
  , parseUnitExpr
  , parseRec
  , parseCorec
  , parseTypeVar
  , parseExprVar
  , parens parseExpr
  ]

parseExprVarT :: Parser Text
parseExprVarT = T.cons
  <$> lowerChar
  <*> takeWhileP Nothing isAlphaNum

parseExprVar :: Parser Expr
parseExprVar = ExprVar
  <$> lexeme parseExprVarT
  -- <*> many (lexeme parseTypeVar)

parseTypeVarT :: Parser Text
parseTypeVarT = T.cons
  <$> upperChar
  <*> takeWhileP Nothing isAlphaNum

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
parseRec = Rec
  <$ symbol "rec"
  <*> lexeme parseExpr
  <* symbol "where"
  <* lexeme newline
  <*> (parseMatch `sepBy` newline)

parseCorec :: Parser Expr
parseCorec = Corec
  <$ symbol "corec"
  <*> lexeme parseExpr
  <* symbol "where"
  <* lexeme newline
  <*> (parseMatch `sepBy` newline)

parseMatch :: Parser Match
parseMatch = Match
  <$> lexeme parseStructorVarT
  <*> lexeme (between "<" ">" (parseExpr `sepBy` symbol ",")
              <|> pure [])
  <*> manyLexeme parseExprVarT
  <* symbol "="
  <*> parseExpr

parseCtxNE :: Parser Ctx
parseCtxNE = ((.).(.)) Map.fromList (:)
  <$ symbol "("
  <*> ((,)
       <$> lexeme parseExprVarT
       <* symbol ":"
       <*> lexeme parseExpr)
  <*> many ((,)
            <$ symbol ","
            <*> lexeme parseExprVarT
            <* symbol ":"
            <*> lexeme parseExpr)
  <* char ')'

parseCtx :: Parser Ctx
parseCtx = parseCtxNE <|> pure Map.empty

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (try parseAbstr)
    ]
  , [ InfixL ((:@:) <$ symbol "@")
    ]
  ]
-- | customized newline which treats ';' as newline
newline :: Parser Char
newline = Parsec.newline <|> char ';'

-- | The space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

manyLexeme :: Parser a -> Parser [a]
manyLexeme = lexeme . many . lexeme

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbols :: [Text] -> Parser [Text]
symbols = traverse symbol

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

withPredicate
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> Text              -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else parseError (FancyError o (Set.singleton (ErrorFail $ T.unpack msg)))

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
