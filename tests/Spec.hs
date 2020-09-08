{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.Megaparsec

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    ((===), forAll, hedgehog)

import Text.Megaparsec

import qualified Data.Map as Map

import Control.Monad.State.Strict

import qualified Data.Text as T

import Lens.Micro.Platform

import Parser
import ShiftFreeVars
import TypeChecker
import TypeAction
import Subst
import Eval
import AbstractSyntaxTree

import Lib
import qualified Bool
import qualified Nat
import qualified Packed
import qualified Maybe
import qualified Pair
import qualified Conat
import qualified List
import qualified Stream

pShow :: Show a => a -> T.Text
pShow = T.pack . show

main :: IO ()
main = hspec $ do
  describe "Parser works" $ do
    it "parses the unit expression" $
      parse parseProgram "" "()" `shouldParse` [Expression UnitExpr]
    it "parses a expression variable" $
      parse parseProgram "" (T.unlines [ "x = ()" , "x"])
      `shouldParse`
      [ ExprDef { name = "x"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = UnitExpr
                , ty = Nothing }
      , Expression $  GlobalExprVar "x" [] []]
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
      [ TypeDef { name = "A"
                , parameterCtx = []
                , typeExpr = In Ductive {
                    gamma = [],
                    strDefs = [StrDef {
                      sigma = [],
                      a = Abstr UnitType UnitType,
                      gamma1 = [],
                      strName = "C"}],
                    nameDuc = ""}
                , kind = Nothing}]
    it "parses a abstraction with multiple arguments" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       -- TODO this should work without brackets
                                       , "   C : ((x:Unit,y:Unit).Unit) -> A"])
      `shouldParse`
      [ TypeDef { name = "A"
                , parameterCtx = []
                , typeExpr = In Ductive {
                    gamma = [],
                    strDefs = [StrDef {
                      sigma = [],
                      a = Abstr UnitType (Abstr UnitType UnitType),
                      gamma1 = [],
                      strName = "C"}],
                    nameDuc = ""}
                , kind = Nothing}]
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
      [ExprDef "x" [] [] UnitExpr Nothing]
    let c = Ductive { gamma = []
                    , strDefs = [StrDef {
                          sigma = []
                        , a = LocalTypeVar 0 "C"
                        , gamma1 = []
                        , strName = "C1"}]
                    , nameDuc = "" }
    it "parses data" $
      parse parseProgram "" "data C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef "C" [] (In c) Nothing]
    it "parses codata" $
      parse parseProgram "" "codata C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef "C" [] (Coin c) Nothing]
    -- TODO Tests for constructors and destructors
    it "parses rec" $
      parse parseProgram "" "data C : Set where { A : C -> C};rec C to Unit where { A x = () }"
      `shouldParse`
      [ TypeDef "C" [] (In c) Nothing
      , Expression $ Rec c UnitType [UnitExpr]]
    it "parses corec" $
      parse parseProgram "" "codata C : Set where { A : C -> C}; corec Unit to C where { A x = ()}"
      `shouldParse`
      [ TypeDef "C" [] (Coin c) Nothing
      , Expression $ Corec UnitType c [UnitExpr]]
    it "parses multiline program" $
      parse parseProgram "" "y = (); y"
      `shouldParse`
      [ ExprDef "y" [] [] UnitExpr Nothing
      , Expression $ GlobalExprVar "y" [] []]
    it "parses a statement with line folding" $
      parse parseProgram "" (T.unlines [ "x ="
                                       , "  ()"
                                       , "   @"
                                       , "    ()"])
      `shouldParse`
       [ ExprDef "x" [] [] (UnitExpr :@: UnitExpr) Nothing]
    it "parses indented data definition" $
      parse parseProgram "" (T.unlines [ "data C : (y:Unit) -> Set where"
                                       , "  C1 : (C @ ()) -> C ()"
                                       , "  C2 : (y:Unit) -> (C @ y) -> C ()"])
      `shouldParse`
      [TypeDef { name = "C"
               , parameterCtx = []
               , typeExpr = In Ductive {
                   gamma = [UnitType],
                   strDefs = [ StrDef { sigma = [UnitExpr]
                                      , a = LocalTypeVar 0 "C" :@ UnitExpr
                                      , gamma1 = []
                                      , strName = "C1"}
                             , StrDef { sigma = [UnitExpr]
                                      , a =  LocalTypeVar 0 "C"
                                             :@ LocalExprVar 0 "y"
                                      , gamma1 = [UnitType]
                                      , strName = "C2"}],
                   nameDuc = ""}
               , kind = Nothing}]
    it "parses a program" $
      parse parseProgram "" (T.unlines [ "x = () @ ()"
                                       , "z = ()"
                                       , "data A : (y:Unit) -> Set where"
                                       , "  C1 : (A @ ()) -> A ()"
                                       , "  C2 : (y:Unit) -> (A @ y) -> A ()"
                                       , "x @ z"])
      `shouldParse`
      [ ExprDef { name = "x"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = UnitExpr :@: UnitExpr
                , ty = Nothing}
      , ExprDef { name = "z"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = UnitExpr
                , ty = Nothing}
      , TypeDef { name = "A"
                , parameterCtx = []
                , typeExpr = In $ Ductive {
                    gamma = [ UnitType],
                    strDefs = [ StrDef { sigma = [UnitExpr],
                                         a = LocalTypeVar 0 "A":@ UnitExpr,
                                         gamma1 = [],
                                         strName = "C1"}
                              , StrDef { sigma = [UnitExpr],
                                         a = LocalTypeVar 0 "A" :@ LocalExprVar 0 "y",
                                         gamma1 = [UnitType],
                                         strName = "C2"}]
                ,   nameDuc = ""}
                , kind = Nothing}
      , Expression $ GlobalExprVar "x" [] [] :@: GlobalExprVar "z" [] []]
    it "parses a rec programm" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       , "  C : A -> A"
                                       , "rec A to A where"
                                       , "  C x = C @ x"
                                       ])
      `shouldParse`
      (let a = Ductive { gamma = []
                       , strDefs = [ StrDef { sigma = []
                                            , a = LocalTypeVar 0 "A"
                                            , gamma1 = []
                                            , strName = "C"} ]
                       , nameDuc = "" }
      in [ TypeDef { name = "A"
                   , parameterCtx = []
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A" []
                          , matches = [ Constructor a 0
                                        :@: LocalExprVar 0 "x" ]}])
    it "parses context with spaces" $
       parse parseProgram "" "data A : (x : Unit, y : Unit) -> Set where"
       `shouldParse`
       [ TypeDef { name = "A"
                 , parameterCtx = []
                 , typeExpr = In Ductive { gamma = [UnitType, UnitType]
                                         , strDefs = []
                                         , nameDuc = ""}
                 , kind = Nothing }]
    it "parses parameters of inductive type" $
      parse parseProgram "" (T.unlines [ "data A<B : Set> : Set where"
                                       , "  C : B -> A"
                                       , "C<Unit>@()"])
      `shouldParse`
      let duc = Ductive { gamma = []
                        , strDefs = [ StrDef { sigma = []
                                             , a = Parameter 0 "B"
                                             , gamma1 = []
                                             , strName = "C"}]
                        , nameDuc = "A"}
      in [ TypeDef { name = "A"
                   , parameterCtx = [[]]
                   , typeExpr = In duc
                   , kind = Nothing}
         , Expression (WithParameters [UnitType] (Constructor { ductive = duc
                                                              , num = 0})
                       :@: UnitExpr)]
    it "parses parameters of coinductive type" $
      parse parseProgram "" (T.unlines [ "codata A<B : Set> : Set where"
                                       , "  C : A -> B"
                                       , "corec<Unit> Unit to A where"
                                       , "  C x = ()"])
      `shouldParse`
      let duc = Ductive { gamma = []
                        , strDefs = [ StrDef { sigma = []
                                             , a = Parameter 0 "B"
                                             , gamma1 = []
                                             , strName = "C"}]
                        , nameDuc = "A" }
      in [ TypeDef { name = "A"
                   , parameterCtx = [[]]
                   , typeExpr = Coin duc
                   , kind = Nothing}
         , Expression (WithParameters [UnitType] (Corec { fromCorec = UnitType
                                                        , toCorec = duc
                                                        , matches = [UnitExpr]}))]
    it "orders matches right" $
       parse parseProgram "" (T.unlines [ "data A : Set where"
                                        , "  C1 : A -> A"
                                        , "  C2 : (x:Unit) -> A -> A"
                                        , "  C3 : (x:Unit,y:Unit) -> A -> A"
                                        , "  C4 : (x:Unit,y:Unit,z:Unit) -> A -> A"
                                        , "rec A to A where"
                                        , "  C3 x y z = C3 @ x @ y @ z"
                                        , "  C2 x y = C2 @ x @ y"
                                        , "  C4 x y z u = C4 @ x @ y @ z @ u"
                                        , "  C1 x  = C1 @ x"
                                        ])
      `shouldParse`
      (let a = Ductive { gamma = []
                       , strDefs = [ StrDef { sigma = []
                                            , a =  LocalTypeVar 0 "A"
                                            , gamma1 = []
                                            , strName = "C1"}
                                   , StrDef { sigma = []
                                            , a =  LocalTypeVar 0 "A"
                                            , gamma1 =  [UnitType]
                                            , strName = "C2"}
                                   , StrDef { sigma = []
                                            , a =  LocalTypeVar 0 "A"
                                            , gamma1 = [UnitType, UnitType]
                                            , strName = "C3"}
                                   , StrDef { sigma = []
                                            , a =  LocalTypeVar 0 "A"
                                            , gamma1 = [UnitType, UnitType, UnitType]
                                            , strName = "C4"}]
                       , nameDuc = "" }
      in [ TypeDef { name = "A"
                   , parameterCtx = []
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A" []
                          , matches = [ Constructor a 0
                                        :@: LocalExprVar 0 "x"
                                      , Constructor a 1
                                        :@: LocalExprVar 1 "x"
                                        :@: LocalExprVar 0 "y"
                                      , Constructor a 2
                                        :@: LocalExprVar 2 "x"
                                        :@: LocalExprVar 1 "y"
                                        :@: LocalExprVar 0 "z"
                                      , Constructor a 3
                                        :@: LocalExprVar 3 "x"
                                        :@: LocalExprVar 2 "y"
                                        :@: LocalExprVar 1 "z"
                                        :@: LocalExprVar 0 "u"
                                      ]}])

  describe "Inlining works" $ do
    let tyA = TypeDef { name = "A"
                      , parameterCtx = []
                      , typeExpr = UnitType
                      , kind = Nothing}
    it "inlines nested Global Var" $
      shouldCheckInGlobCtx [tyA]
                           (inlineTypeExpr $ GlobalTypeVar "A" [])
                           UnitType
    let tyB = TypeDef { name = "B"
                      , parameterCtx = []
                      , typeExpr = GlobalTypeVar "A" []
                      , kind = Nothing}
    it "inlines nested Global Var" $
      shouldCheckInGlobCtx [tyA,tyB]
                           (inlineTypeExpr $ GlobalTypeVar "A" [])
                           UnitType
    let tyC = TypeDef { name = "C"
                      , parameterCtx = [[]]
                      , typeExpr = Parameter 0 "X"
                      , kind = Nothing}
    it "inlines parameterized Global Var" $
      shouldCheckInGlobCtx [tyC]
                           (inlineTypeExpr $ GlobalTypeVar "C" [UnitType])
                           UnitType
    let a = ExprDef { name = "a"
                    , tyParameterCtx = []
                    , exprParameterCtx = []
                    , expr = UnitExpr
                    , ty = Nothing}
    it "inlines Global var with unit expr" $
      shouldCheckInGlobCtx [a]
                           (inlineExpr $ GlobalExprVar "a" [] [])
                           UnitExpr

  let emptyDuc = Ductive { gamma = []
                         , strDefs = []
                         , nameDuc = ""}

  describe "Shifting of free variables works" $ do
    it "shift free vars doesn't change UnitType" $ hedgehog $ do
      n <- forAll $ Gen.integral (Range.linear 0 100)
      m <- forAll $ Gen.integral (Range.linear 0 100)
      shiftFreeVarsTypeExpr n m UnitType === UnitType
    it "shift free vars doesn't change Abstraction" $ hedgehog $ do
      n <- forAll $ Gen.integral (Range.linear 0 100)
      m <- forAll $ Gen.integral (Range.linear 0 100)
      shiftFreeVarsTypeExpr n m (Abstr UnitType UnitType)
        ===
        Abstr UnitType UnitType

  describe "Type Checker works" $ do
    let shouldCheck :: (HasCallStack, Show a, Eq a) => TI ann a -> a -> Expectation
        shouldCheck ti = shouldCheckIn ti emptyCtx
    it "type checks unit type" $
      inferType UnitType
      `shouldCheck`
      []
    it "type checks abstraction" $
      inferType (Abstr UnitType UnitType)
      `shouldCheck`
      [UnitType]
    it "type checks application" $
      inferType (Abstr UnitType UnitType :@ UnitExpr)
      `shouldCheck`
      []
    it "type checks mu" $
      inferType (In emptyDuc)
      `shouldCheck`
      []
    it "type checks nu" $
      inferType (Coin emptyDuc)
      `shouldCheck`
      []
    it "type checks unit expr" $
      inferTerm UnitExpr
      `shouldCheck`
      ([],UnitType)
    it "type checks rec" $
      inferTerm (Rec { fromRec = emptyDuc
                     , toRec = UnitType
                     , matches = []})
      `shouldCheck`
      ([In emptyDuc],UnitType)
    it "type checks corec" $
      inferTerm (Corec { fromCorec = UnitType
                       , toCorec = emptyDuc
                       , matches = []})
      `shouldCheck`
      ([UnitType], Coin emptyDuc)

  -- Complete programs
  Bool.tests
  Packed.tests
  Maybe.tests
  Pair.tests
  Nat.tests
  Conat.tests
  List.tests
  Stream.tests
