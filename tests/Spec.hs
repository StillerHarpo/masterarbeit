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
      [ TypeDef { name = "A"
                , typeExpr = In Ductive {
                    gamma = [],
                    sigmas = [[]],
                    as = [ Abstr UnitType UnitType ],
                    gamma1s = [[]],
                    nameDuc = Nothing}
                , kind = Nothing}]
    it "parses a abstraction with multiple arguments" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       -- TODO this should work without brackets
                                       , "   C : ((x:Unit,y:Unit).Unit) -> A"])
      `shouldParse`
      [ TypeDef { name = "A"
                , typeExpr = In Ductive {
                    gamma = [],
                    sigmas = [[]],
                    as = [ Abstr UnitType (Abstr UnitType UnitType)],
                    gamma1s = [[]],
                    nameDuc = Nothing}
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
      [ExprDef "x" UnitExpr Nothing]
    let c = Ductive { gamma = []
                    , sigmas = [[]]
                    , as = [LocalTypeVar 0]
                    , gamma1s = [[]]
                    , nameDuc = Nothing}
    it "parses data" $
      parse parseProgram "" "data C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef "C" (In c) Nothing]
    it "parses codata" $
      parse parseProgram "" "codata C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef "C" (Coin c) Nothing]
    -- TODO Tests for constructors and destructors
    it "parses rec" $
      parse parseProgram "" "data C : Set where { A : C -> C};rec C to Unit where { A x = () }"
      `shouldParse`
      [ TypeDef "C" (In c) Nothing
      , Expression $ Rec c UnitType [UnitExpr]]
    it "parses corec" $
      parse parseProgram "" "codata C : Set where { A : C -> C}; corec Unit to C where { A x = ()}"
      `shouldParse`
      [ TypeDef "C" (Coin c) Nothing
      , Expression $ Corec UnitType c [UnitExpr]]
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
      [TypeDef { name = "C"
               , typeExpr = In Ductive {
                   gamma = [UnitType],
                   sigmas = [ [UnitExpr]
                            , [UnitExpr]],
                   as = [ LocalTypeVar 0 :@ LocalExprVar 0
                        , LocalTypeVar 0 :@ UnitExpr ],
                   gamma1s = [ []
                             , []],
                   nameDuc = Nothing}
               , kind = Nothing}]
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
      , TypeDef { name = "A"
                , typeExpr = In $ Ductive {
                    gamma = [ UnitType],
                    sigmas = [ [UnitExpr]
                             , [UnitExpr]],
                    as = [ LocalTypeVar 0 :@ LocalExprVar 0
                         , LocalTypeVar 0 :@ UnitExpr],
                    gamma1s = [ []
                              , []],
                    nameDuc = Nothing}
                , kind = Nothing}
      , Expression $ GlobalExprVar "x" :@: GlobalExprVar "z" ]
    it "parses a rec programm" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       , "  C : A -> A"
                                       , "rec A to A where"
                                       , "  C x = C @ x"
                                       ])
      `shouldParse`
      (let a = Ductive { gamma = []
                       , sigmas = [[]]
                       , as = [LocalTypeVar 0]
                       , gamma1s = [[]]
                       , nameDuc = Nothing}
      in [ TypeDef { name = "A"
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A"
                          , matches = [ Constructor a 0 Nothing :@: LocalExprVar 0 ]}])
    it "orders matches right" $
       parse parseProgram "" (T.unlines [ "data A : Set where"
                                        , "  C1 : A -> A"
                                        , "  C2 : (x:Unit) -> A -> A"
                                        -- TODO allow space in contexts
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
                       , sigmas = [[],[],[],[]]
                       , as = [ LocalTypeVar 0
                              , LocalTypeVar 0
                              , LocalTypeVar 0
                              , LocalTypeVar 0]
                       , gamma1s = [ [UnitType, UnitType, UnitType]
                                   , [UnitType, UnitType]
                                   , [UnitType]
                                   , []]
                       , nameDuc = Nothing}
      in [ TypeDef { name = "A"
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A"
                          , matches = [ Constructor a 0 Nothing
                                        :@: LocalExprVar 3
                                        :@: LocalExprVar 2
                                        :@: LocalExprVar 1
                                        :@: LocalExprVar 0
                                      , Constructor a 1 Nothing
                                        :@: LocalExprVar 2
                                        :@: LocalExprVar 1
                                        :@: LocalExprVar 0
                                      , Constructor a 2 Nothing
                                        :@: LocalExprVar 1
                                        :@: LocalExprVar 0
                                      , Constructor a 3 Nothing
                                        :@: LocalExprVar 0]}])

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
    let natDuc = Ductive { gamma = []
                         , sigmas = [[],[]]
                         , as = [UnitType, LocalTypeVar 0]
                         , gamma1s = [[],[]]
                         , nameDuc = Just "nat"}
        nat = In natDuc
        idNat = Rec { fromRec = natDuc
                    , toRec = nat
                    , matches = [ Constructor natDuc 0 (Just "Z") :@: LocalExprVar 0
                                , Constructor natDuc 1 (Just "S") :@: LocalExprVar 0]
                    }
        zero = Constructor natDuc 0 (Just "Z"):@: UnitExpr
        one = Constructor natDuc 1 (Just "S") :@: zero
    it "evaluates the identity function to the identity function" $
      evalExpr idNat
      `shouldCheck`
      idNat
    it "evaluates the zero to the zero" $
      evalExpr zero
      `shouldCheck`
      zero
    it "infer type of expr UnitExpr yiels UnitType" $
      inferTerm UnitExpr
      `shouldCheck`
      ([], UnitType)
    it "typeaction works" $
      typeAction 0 UnitType [idNat :@: LocalExprVar 0] [[]] [UnitType] [nat]
      `shouldBe`
      LocalExprVar 0
    it "evaluates the identity function on zero to zero" $
      evalExpr (idNat :@: zero)
      `shouldCheck`
      zero
    it "evaluates the one to the one" $
      evalExpr one
      `shouldCheck`
      one
    it "infer type of expr zero yiels Nat" $
      inferTerm zero
      `shouldCheck`
      ([], nat)
    it "infer type of expr one yiels Nat" $
      inferTerm one
      `shouldCheck`
      ([], nat)
    it "typeaction works on typeVar" $
      typeAction 0 (LocalTypeVar 0) [idNat :@: LocalExprVar 0] [[]] [nat] [nat]
      `shouldBe`
      idNat :@: LocalExprVar 0
    it "substExpr works" $
      substExpr 0 (idNat :@: LocalExprVar 0) (Constructor natDuc 1 (Just "S") :@: LocalExprVar 0)
      `shouldBe`
      Constructor natDuc 1 (Just "S") :@: (idNat :@: LocalExprVar 0)
    it "substExprs works" $
      substExprs 0 [zero] (Constructor natDuc 1 (Just "S") :@: (idNat :@: LocalExprVar 0))
      `shouldBe`
      (Constructor natDuc 1 (Just "S") :@: (idNat :@: zero))
    it "evaluates the identity function on one to one" $
      evalExpr (idNat :@: one)
      `shouldCheck`
      one
