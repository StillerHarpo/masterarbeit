{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Data.Map as Map

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
    it "parses data" $
      parse parseStatement "" "data A : Set where; C1 : A -> A"
      `shouldParse`
      DataVariable "A" ["C1"] (Inductive Map.empty [[]] [TypeVar "A"] [Map.empty])
    it "parses codata" $
      parse parseStatement "" "codata A : Set where; C1 : A -> A"
      `shouldParse`
      DataVariable "A" ["C1"] (Coinductive Map.empty [[]] [TypeVar "A"] [Map.empty])
    it "parses judgments" $
      parse parseJudgment "" "y = (); y"
      `shouldParse`
      Judgment (Map.fromList [("y",UnitExpr)])
               Map.empty
               Map.empty
               (ExprVar "y")
