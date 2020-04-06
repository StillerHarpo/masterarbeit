{-# language TupleSections #-}
{-# language OverloadedStrings #-}
module Parser where

import AbstractSyntaxTree

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (void)

import Data.Char (isAlphaNum)
import Data.Text (Text,cons)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Variable = ExprVariable Text Expr
              | DataVariable Text [Text] Expr

-- | The space consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbols :: [Text] -> Parser [Text]
symbols = traverse symbol

parseExprVarT :: Parser Text
parseExprVarT = cons
  <$> lowerChar
  <*> takeWhileP Nothing isAlphaNum

parseExprVar :: Parser Expr
parseExprVar = ExprVar
  <$> lexeme parseExprVarT
  -- <*> many (lexeme parseTypeVar)

parseTypeVarT :: Parser Text
parseTypeVarT = cons
  <$> upperChar
  <*> takeWhile1P Nothing isAlphaNum

parseStructorVarT :: Parser Text
parseStructorVarT = parseTypeVarT

parseTypeVar :: Parser Expr
parseTypeVar = TypeVar <$> parseTypeVarT

parseUnitType :: Parser Expr
parseUnitType = UnitType <$ string "Unit"

parseUnitExpr :: Parser Expr
parseUnitExpr = UnitExpr <$ string "()"


parseDefinition :: Parser Variable
parseDefinition = ExprVariable
  <$> lexeme parseTypeVarT
  <* symbol "="
  <*> parseExpr

parseCtx :: Parser Ctx
parseCtx = Map.fromList
  <$ symbol "("
  <*> many ((,) <$> lexeme parseExprVarT <* symbol "," <*> lexeme parseExpr)
  <* char ')'

withPredicate
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else parseError (FancyError o (Set.singleton (ErrorFail msg)))

parseConstructorDef :: Text -> Parser (Text, Ctx, Expr, [Expr])
parseConstructorDef name = (,,,)
  <$> lexeme parseStructorVarT
  <* symbol ":"
  <*> lexeme parseCtx
  <* symbol "->"
  <*> lexeme parseExpr
  <* symbol "->"
  <* lexeme (withPredicate (== name) "Should be the same as type" parseTypeVarT)
  <*> many (lexeme parseExpr)

parseDestructorDef :: Text -> Parser (Text, Ctx, [Expr], Expr)
parseDestructorDef name = (,,,)
  <$> lexeme parseStructorVarT
  <* symbol ":"
  <*> lexeme parseCtx
  <* symbol "->"
  <* lexeme (withPredicate (== name) "Should be the same as type" parseTypeVarT)
  <*> many (lexeme parseExpr)
  <* symbol "->"
  <*> lexeme parseExpr

unzip4 :: [(a,b,c,d)] -> ([a], [b], [c], [d])
unzip4 = foldl (\(as, bs, cs, ds) (a, b ,c ,d) -> (a:as,b:bs,c:cs,d:ds))
               ([],[],[],[])

parseData :: Parser Variable
parseData = do
  void $ symbol "data"
  nameX <- lexeme parseTypeVarT
  typeParameters <- many (lexeme parseTypeVarT)
  gamma <- parseCtx
  void $ symbols ["->", "Set", "where"]
  void $ lexeme $ newline <|> char ';'
  constructors <- many (lexeme (parseConstructorDef nameX)
                        <* lexeme (newline <|> char ';')) -- Constructors
  let (constrNames, gamma1s, as, sigmas) = unzip4 constructors
  pure $ DataVariable nameX constrNames $ Inductive gamma sigmas as gamma1s

parseCodata :: Parser Variable
parseCodata = do
  void $ symbol "codata"
  nameX <- lexeme parseTypeVarT
  typeParameters <- many (lexeme parseTypeVarT)
  gamma <- parseCtx
  void $ symbols ["->", "Set", "where"]
  void $ lexeme $ newline <|> char ';'
  destructors <- many (lexeme (parseDestructorDef nameX)
                        <* lexeme (newline <|> char ';')) -- Constructors
  let (constrNames, gamma1s, sigmas, as) = unzip4 destructors
  pure $ DataVariable nameX constrNames $ Coinductive gamma sigmas as gamma1s

parseExpr :: Parser Expr
parseExpr = parseUnitExpr
          <|> parseUnitType
          <|> parseTypeVar
          <|> parseExprVar

parseStatement :: Parser Variable
parseStatement = parseData
               <|> parseDefinition

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

parseJudgment :: Parser Judgment
parseJudgment = buildJudgment
  <$> many (parseStatement <* newline)
  <*> parseExpr
