{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
-- import Text.Megaparserc.Char

import Parser
import AbstractSyntaxTree

main :: IO ()
main = hspec $ do
  describe "Parser works" $ do
    it "parses the unit expression" $
      parse parseExpr "" "()" `shouldParse` UnitExpr
    it "parses the unit type" $
      parse parseExpr "" "Unit" `shouldParse` UnitType
    it "parses a expression variable" $
      parse parseExpr "" "x1" `shouldParse`  ExprVar "x1"
    it "parses a expression variable" $
      parse parseExpr "" "C1" `shouldParse`  TypeVar "C1"
    it "parses a application" $
      parse parseExpr "" "x @ y"
      `shouldParse`
      (ExprVar "x" :@: ExprVar "y")
    it "parses a abstraction" $
      parse parseExpr "" "(x:Unit).()"
      `shouldParse`
      Abstr "x" UnitType UnitExpr
    it "parses a abstraction with multiple arguments" $
      parse parseExpr "" "(x:Unit,y:Unit).()"
      `shouldParse`
      Abstr "x" UnitType (Abstr "y" UnitType UnitExpr)
    it "parse a application left associative" $
      parse parseExpr "" "x @ z @ y"
      `shouldParse`
      (ExprVar "x" :@: ExprVar "z" :@: ExprVar "y")
    it "parse a expression with brackets" $
      parse parseExpr "" "x @ (z @ y)"
      `shouldParse`
      (ExprVar "x" :@: (ExprVar "z" :@: ExprVar "y"))
    it "parses definitions" $
      parse parseStatement "" "x = ()"
      `shouldParse`
      ExprVariable "x" UnitExpr
