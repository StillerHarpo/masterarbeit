{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.Megaparsec

import Text.Megaparsec

import qualified Data.Map as Map

import Control.Monad.State.Strict

import qualified Data.Text as T

import Parser
import TypeChecker
import AbstractSyntaxTree

main :: IO ()
main = hspec $ do
  describe "Parser works" $ do
    it "parses the unit expression" $
      parse parseProgram "" "()" `shouldParse` [Expression UnitExpr]
    it "parses a expression variable" $
      parse parseProgram "" (T.unlines [ "x = ()" , "x"])
      `shouldParse`
      [ ExprDef { name = "x"
                , expr = UnitExpr
                , ty = Nothing }
      , Expression $  GlobalExprVar "x"]
    it "parses a application" $
      parse parseProgram "" "() @ ()"
      `shouldParse`
      [Expression $  UnitExpr :@: UnitExpr]
    it "parses a application with brackets" $
      parse parseProgram "" "(() @ ())"
      `shouldParse`
      [Expression $  UnitExpr :@: UnitExpr]
    it "parses a abstraction" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       -- TODO this should work without brackets
                                       , "   C : ((x:Unit).Unit) -> A"])
      `shouldParse`
      [ InductiveDef { name = "A"
                     , gamma = []
                     , sigmas = [[]]
                     , as = [ Abstr UnitType UnitType ]
                     , constructors = [ "C"]
                     , gamma1s = [[]]
                     }]
    it "parses a abstraction with multiple arguments" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       -- TODO this should work without brackets
                                       , "   C : ((x:Unit,y:Unit).Unit) -> A"])
      `shouldParse`
      [ InductiveDef { name = "A"
                     , gamma = []
                     , sigmas = [[]]
                     , as = [ Abstr UnitType (Abstr UnitType UnitType)]
                     , constructors = [ "C"]
                     , gamma1s = [[]]
                     }]
    it "parse a application left associative" $
      parse parseProgram "" "() @ () @ ()"
      `shouldParse`
      [Expression $ UnitExpr :@: UnitExpr :@: UnitExpr]
    it "parse a expression with brackets" $
      parse parseProgram "" "() @ (() @ ())"
      `shouldParse`
      [Expression $ UnitExpr :@: (UnitExpr :@: UnitExpr)]
    it "parses definitions" $
      parse parseProgram "" "x = ()"
      `shouldParse`
      [ExprDef "x" UnitExpr Nothing]
    it "parses data" $
      parse parseProgram "" "data C : Set where { C1 : C -> C }"
      `shouldParse`
      [InductiveDef "C" [] [[]] [Inductive "C"] ["C1"] [[]]]
    it "parses codata" $
      parse parseProgram "" "codata C : Set where { C1 : C -> C }"
      `shouldParse`
      [CoinductiveDef "C" [] [[]] [Coinductive "C"] ["C1"] [[]]]
    -- TODO Tests for constructors and destructors
    it "parses rec" $
      parse parseProgram "" "rec C to Unit where { A x = () }"
      `shouldParse`
      [Expression $ Rec "C" UnitType [Match "A" UnitExpr]]
    it "parses corec" $
      parse parseProgram "" "corec Unit to C where { A x = ()}"
      `shouldParse`
      [Expression $ Corec UnitType "C" [Match "A"  UnitExpr]]
    it "parses multiline program" $
      parse parseProgram "" "y = (); y"
      `shouldParse`
      [ ExprDef "y" UnitExpr Nothing
      , Expression $ GlobalExprVar "y"]
    it "parses a statement with line folding" $
      parse parseProgram "" (T.unlines [ "x ="
                                       , "  ()"
                                       , "   @"
                                       , "    ()"])
      `shouldParse`
       [ ExprDef "x" (UnitExpr :@: UnitExpr) Nothing]
    it "parses indented data definition" $
      parse parseProgram "" (T.unlines [ "data C : (y:Unit) -> Set where"
                                       , "  C1 : (C @ ()) -> C ()"
                                       , "  C2 : (C @ y) -> C ()"])
      `shouldParse`
      [InductiveDef { name = "C"
                    , gamma = [UnitType]
                    , sigmas = [ [UnitExpr]
                               , [UnitExpr]]
                    , as = [ Inductive "C" :@ LocalExprVar 0
                           , Inductive "C" :@ UnitExpr ]
                    , constructors = [ "C2"
                                     , "C1"]
                    , gamma1s = [ []
                                , []]}]
    it "parses a program" $
      parse parseProgram "" (T.unlines [ "x = () @ ()"
                                       , "z = ()"
                                       , "data A : (y:Unit) -> Set where"
                                       , "  C1 : (A @ ()) -> A ()"
                                       , "  C2 : (A @ y) -> A ()"
                                       , "x @ z"])
      `shouldParse`
      [ ExprDef { name = "x"
                , expr = UnitExpr :@: UnitExpr
                , ty = Nothing}
      , ExprDef { name = "z"
                , expr = UnitExpr
                , ty = Nothing}
      , InductiveDef { name = "A"
                     , gamma = [ UnitType]
                     , sigmas = [ [UnitExpr]
                                , [UnitExpr]]
                     , as = [ Inductive "A" :@ LocalExprVar 0
                            , Inductive "A" :@ UnitExpr]
                     , constructors = [ "C2"
                                      , "C1"]
                     , gamma1s = [ []
                                 , []]}
      , Expression $ GlobalExprVar "x" :@: GlobalExprVar "z" ]
    it "parses a rec programm" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       , "  C : A -> A"
                                       , "rec A to A where"
                                       , "  C x = C @ x"
                                       ])
      `shouldParse`
      [ InductiveDef { name = "A"
                     , gamma = []
                     , sigmas = [[]]
                     , as = [ Inductive "A"]
                     , constructors = [ "C"]
                     , gamma1s = [[]]
                     }
      , Expression Rec { fromRec = "A"
                       , toRec = Inductive "A"
                       , matches = [ Match { structorName = "C"
                                           , matchExpr = Constructor "C" :@: LocalExprVar 0
                                           }]}]

  describe "Type Checker works" $ do
    let shouldCheck :: (HasCallStack, Show a, Eq a) => TI a -> a -> Expectation
        shouldCheck ti val = runTI ti (ContextTI { _ctx = []
                                                 , _tyCtx = []
                                                 , _defCtx = []
                                                 , _strCtx = []})
                             `shouldBe`
                             Right val
    it "type check unit type" $
      inferType UnitType
      `shouldCheck`
      []
    it "type check unit expr" $
      inferTerm UnitExpr
      `shouldCheck`
      ([],UnitType)
