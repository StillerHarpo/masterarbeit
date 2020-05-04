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
    it "parses the unit type" $
      parse parseProgram "" "Unit" `shouldParse` [Expression UnitType]
    it "parses a expression variable" $
      parse parseProgram "" "(x:Unit).x" `shouldParse`  [Expression $ Abstr UnitType $ LocalExprVar 0]
    it "parses a type variable" $
      parse parseProgram "" "A" `shouldParse`  [Expression $ TypeVar "A"]
    it "parses a application" $
      parse parseProgram "" "(x:Unit,y:Unit).x @ y"
      `shouldParse`
      [Expression $ Abstr UnitType $ Abstr UnitType $ LocalExprVar 1 :@: LocalExprVar 0]
    it "parses a application 2" $
      parse parseProgram "" "(x:Unit).A @ x"
      `shouldParse`
      [Expression $ Abstr UnitType $ TypeVar "A" :@: LocalExprVar 0]
    it "parses a application with brackets" $
      parse parseProgram "" "(x:Unit).(A @ x)"
      `shouldParse`
      [Expression $ Abstr UnitType $ TypeVar "A" :@: LocalExprVar 0]
    it "parses a abstraction" $
      parse parseProgram "" "(x:Unit).()"
      `shouldParse`
      [Expression $ Abstr UnitType UnitExpr]
    it "parses a abstraction with multiple arguments" $
      parse parseProgram "" "(x:Unit,y:Unit).()"
      `shouldParse`
      [Expression $ Abstr  UnitType (Abstr UnitType UnitExpr)]
    it "parse a application left associative" $
      parse parseProgram "" "(x:Unit,y:Unit,z:Unit). x @ y @ z"
      `shouldParse`
      [Expression $ Abstr UnitType $ Abstr UnitType $ Abstr UnitType $
       LocalExprVar 2 :@: LocalExprVar 1 :@: LocalExprVar 0]
    it "parse a expression with brackets" $
      parse parseProgram "" "(x:Unit,y:Unit,z:Unit).x @ (y @ z)"
      `shouldParse`
      [Expression $ Abstr UnitType $ Abstr UnitType $ Abstr UnitType $
       LocalExprVar 2 :@: (LocalExprVar 1 :@: LocalExprVar 0)]
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
      [Expression $ Rec (TypeVar "C") UnitType [Match "A" UnitExpr]]
    it "parses corec" $
      parse parseProgram "" "corec Unit to C where { A x = ()}"
      `shouldParse`
      [Expression $ Corec UnitType (TypeVar "C") [Match "A"  UnitExpr]]
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
                    , as = [ Inductive "C" :@: LocalExprVar 0
                           , Inductive "C" :@: UnitExpr ]
                    , constructors = [ "C2"
                                     , "C1"]
                    , gamma1s = [ []
                                , []]}]
    it "parses a program" $
      parse parseProgram "" (T.unlines [ "x = (y:Unit).y"
                                       , "z = ()"
                                       , "data A : (y:Unit) -> Set where"
                                       , "  C1 : (A @ ()) -> A ()"
                                       , "  C2 : (A @ y) -> A ()"
                                       , "x @ z"])
      `shouldParse`
      [ ExprDef { name = "x"
                , expr = Abstr UnitType (LocalExprVar 0)
                , ty = Nothing}
      , ExprDef { name = "z"
                , expr = UnitExpr
                , ty = Nothing}
      , InductiveDef { name = "A"
                     , gamma = [ UnitType]
                     , sigmas = [ [UnitExpr]
                                , [UnitExpr]]
                     , as = [ Inductive "A" :@: LocalExprVar 0
                            , Inductive "A" :@: UnitExpr]
                     , constructors = [ "C2"
                                      , "C1"]
                     , gamma1s = [ []
                                 , []]}
      , Expression $ GlobalExprVar "x" :@: GlobalExprVar "z" ]

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
