{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
-- import Text.Megaparserc.Char

import Parser
import AbstractSyntaxTree

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses the unit expression" $
      parse parseExpr "" "()" `shouldParse` UnitExpr
    it "parses the unit type" $
      parse parseExpr "" "Unit" `shouldParse` UnitType
    it "parses a expression variable" $
      parse parseExpr "" "x1" `shouldParse`  ExprVar "x1"
    it "parses a expression variable" $
      parse parseExpr "" "C1" `shouldParse`  TypeVar "C1"
    it "parses a application" $
      parse parseExpr "" "x @ y" `shouldParse` (ExprVar "x" :@: ExprVar "y")
