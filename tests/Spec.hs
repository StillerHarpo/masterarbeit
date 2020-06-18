{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.Megaparsec

import Text.Megaparsec

import qualified Data.Map as Map

import Control.Monad.State.Strict

import qualified Data.Text as T

import Lens.Micro.Platform

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
                    , as = [LocalTypeVar 0 (Just "C")]
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
                   as = [ LocalTypeVar 0 (Just "C")
                          :@ LocalExprVar 0 (Just "y")
                        , LocalTypeVar 0 (Just "C") :@ UnitExpr ],
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
                    as = [ LocalTypeVar 0 (Just "A") :@ LocalExprVar 0 (Just "y")
                         , LocalTypeVar 0 (Just "A"):@ UnitExpr],
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
                       , as = [LocalTypeVar 0 (Just "A")]
                       , gamma1s = [[]]
                       , nameDuc = Nothing}
      in [ TypeDef { name = "A"
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A"
                          , matches = [ Constructor a 0 Nothing
                                        :@: LocalExprVar 0 (Just "x") ]}])
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
                       , as = [ LocalTypeVar 0 (Just "A")
                              , LocalTypeVar 0 (Just "A")
                              , LocalTypeVar 0 (Just "A")
                              , LocalTypeVar 0 (Just "A")]
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
                                        :@: LocalExprVar 3 (Just "x")
                                        :@: LocalExprVar 2 (Just "y")
                                        :@: LocalExprVar 1 (Just "z")
                                        :@: LocalExprVar 0 (Just "u")
                                      , Constructor a 1 Nothing
                                        :@: LocalExprVar 2 (Just "x")
                                        :@: LocalExprVar 1 (Just "y")
                                        :@: LocalExprVar 0 (Just "z")
                                      , Constructor a 2 Nothing
                                        :@: LocalExprVar 1 (Just "x")
                                        :@: LocalExprVar 0 (Just "y")
                                      , Constructor a 3 Nothing
                                        :@: LocalExprVar 0 (Just "x")]}])

  describe "Type Checker works" $ do
    let emptyCtx :: ContextTI
        emptyCtx = ContextTI { _ctx = []
                             , _tyCtx = []
                             , _defCtx = []
                             , _strCtx = []}
        shouldCheckIn :: (HasCallStack, Show a, Eq a) => TI a -> ContextTI -> a -> Expectation
        shouldCheckIn ti ctx' val = runTI ti ctx' `shouldBe` Right val
        shouldCheck :: (HasCallStack, Show a, Eq a) => TI a -> a -> Expectation
        shouldCheck ti = shouldCheckIn ti emptyCtx
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
                         , as = [UnitType, LocalTypeVar 0 (Just "X")]
                         , gamma1s = [[],[]]
                         , nameDuc = Just "nat"}
        nat = In natDuc
        idNat = Rec { fromRec = natDuc
                    , toRec = nat
                    , matches = [ Constructor natDuc 0 (Just "Z")
                                  :@: LocalExprVar 0 (Just "y1")
                                , Constructor natDuc 1 (Just "S")
                                  :@: LocalExprVar 0 (Just "y2")]
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
      typeAction 0 UnitType
                   [idNat :@: LocalExprVar 0 Nothing]
                   [[]] [UnitType] [nat]
      `shouldBe`
      LocalExprVar 0 Nothing
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
      typeAction 0 (LocalTypeVar 0 Nothing)
                   [idNat :@: LocalExprVar 0 Nothing]
                   [[]] [nat] [nat]
      `shouldBe`
      idNat :@: LocalExprVar 0 Nothing
    it "substExpr works" $
      substExpr 0 (idNat :@: LocalExprVar 0 Nothing)
                  (Constructor natDuc 1 (Just "S")
                   :@: LocalExprVar 0 Nothing)
      `shouldBe`
      Constructor natDuc 1 (Just "S")
      :@: (idNat :@: LocalExprVar 0 Nothing)
    it "substExprs works" $
      substExprs 0 [zero] (Constructor natDuc 1 (Just "S")
                           :@: (idNat :@: LocalExprVar 0 Nothing))
      `shouldBe`
      (Constructor natDuc 1 (Just "S") :@: (idNat :@: zero))
    it "evaluates the identity function on one to one" $
      evalExpr (idNat :@: one)
      `shouldCheck`
      one
    -- vector without pair typ, just save the element in gamma2
    let suc = Constructor natDuc 1 (Just "S")
        vec2Duc = Ductive { gamma = [ nat ]
                          , sigmas = [ [zero]
                                     , [suc :@: LocalExprVar 2 (Just "k")]]
                          , as = [ UnitType
                                 , LocalTypeVar 0 (Just "X") :@ LocalExprVar 1 (Just "k")]
                          , gamma1s = [[],[nat, UnitType]]
                          , nameDuc = Just "vec2"}

        vec2 = In vec2Duc
        emptyVec2 = Constructor vec2Duc 0 (Just "[]") :@: UnitExpr
        oneVec2 = Constructor vec2Duc 1 (Just "::") :@: zero :@: UnitExpr
                 :@: emptyVec2
        idVec2 = Rec { fromRec = vec2Duc
                     , toRec = vec2
                     , matches = [ Constructor vec2Duc 0 (Just "[]")
                                   :@: LocalExprVar 0 (Just "y1")
                                 , Constructor vec2Duc 1 (Just "::")
                                   :@: LocalExprVar 2 (Just "k")
                                   :@: LocalExprVar 1 (Just "x")
                                   :@: LocalExprVar 0 (Just "y2")]
                     }
    it "infers type vec2 for emptyVec2" $
      inferTerm emptyVec2
      `shouldCheck`
      ([],vec2 :@ zero)
    it "infers type vec2 for oneVec2" $
      inferTerm oneVec2
      `shouldCheck`
      ([],vec2 :@ one)
    it "infers type vec2 for identity function on oneVec2" $
      inferTerm (idVec2 :@: one :@: oneVec2)
      `shouldCheck`
      ([],vec2 :@ one)
