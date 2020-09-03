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
  Bool.tests
  Nat.tests
  Packed.tests
  Maybe.tests
  Pair.tests
  Conat.tests
  List.tests
  Stream.tests
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
                , parameterCtx = []
                , typeExpr = In Ductive {
                    gamma = [],
                    sigmas = [[]],
                    as = [ Abstr UnitType UnitType ],
                    gamma1s = [[]],
                    nameDuc = "",
                    strNames = ["C"]}
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
                    sigmas = [[]],
                    as = [ Abstr UnitType (Abstr UnitType UnitType)],
                    gamma1s = [[]],
                    nameDuc = "",
                    strNames = ["C"]}
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
                    , as = [LocalTypeVar 0 "C"]
                    , gamma1s = [[]]
                    , nameDuc = ""
                    , strNames = ["C1"]}
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
                                       , "  C2 : (y:Unit) -> (C @ y) -> C ()"])
      `shouldParse`
      [TypeDef { name = "C"
               , parameterCtx = []
               , typeExpr = In Ductive {
                   gamma = [UnitType],
                   sigmas = [ [UnitExpr]
                            , [UnitExpr]],
                   as = [ LocalTypeVar 0 "C" :@ UnitExpr
                        , LocalTypeVar 0 "C"
                          :@ LocalExprVar 0 "y"],
                   gamma1s = [ []
                             , [UnitType]],
                   nameDuc = "",
                   strNames = ["C1", "C2"]}
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
                , expr = UnitExpr :@: UnitExpr
                , ty = Nothing}
      , ExprDef { name = "z"
                , expr = UnitExpr
                , ty = Nothing}
      , TypeDef { name = "A"
                , parameterCtx = []
                , typeExpr = In $ Ductive {
                    gamma = [ UnitType],
                    sigmas = [ [UnitExpr]
                             , [UnitExpr]],
                    as = [ LocalTypeVar 0 "A":@ UnitExpr
                         , LocalTypeVar 0 "A" :@ LocalExprVar 0 "y"],
                    gamma1s = [ []
                              , [UnitType]],
                    nameDuc = "",
                    strNames = ["C1", "C2"]}
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
                       , as = [LocalTypeVar 0 "A"]
                       , gamma1s = [[]]
                       , nameDuc = ""
                       , strNames = ["C"]}
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
                                         , sigmas = []
                                         , as = []
                                         , gamma1s = []
                                         , nameDuc = ""
                                         , strNames = []}
                 , kind = Nothing }]
    it "parses parameters of inductive type" $
      parse parseProgram "" (T.unlines [ "data A<B : Set> : Set where"
                                       , "  C : B -> A"
                                       , "C<Unit>@()"])
      `shouldParse`
      let duc = Ductive { gamma = []
                        , sigmas = [[]]
                        , as = [Parameter 0 "B"]
                        , gamma1s = [[]]
                        , nameDuc = "A"
                        , strNames = ["C"]}
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
                        , sigmas = [[]]
                        , as = [Parameter 0 "B"]
                        , gamma1s = [[]]
                        , nameDuc = "A"
                        , strNames = ["C"]}
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
                       , sigmas = [[],[],[],[]]
                       , as = [ LocalTypeVar 0 "A"
                              , LocalTypeVar 0 "A"
                              , LocalTypeVar 0 "A"
                              , LocalTypeVar 0 "A"]
                       , gamma1s = [ []
                                   , [UnitType]
                                   , [UnitType, UnitType]
                                   , [UnitType, UnitType, UnitType]]
                       , nameDuc = ""
                       , strNames = ["C1", "C2", "C3", "C4"]}
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
    it "parse pairs" $
       parse parseProgram "" (T.unlines [ "data Pair : Set where"
                                        , "  MkPair : (x:Unit) -> Unit -> Pair"
                                        , "MkPair @ () @ ()"
                                        ])
      `shouldParse`
      (let pairDuc = Ductive { gamma = []
                             , sigmas = [[]]
                             , as = [ UnitType ]
                             , gamma1s = [ [UnitType] ]
                             , nameDuc = ""
                             , strNames = ["MkPair"]}
       in [ TypeDef { name = "Pair"
                    , parameterCtx = []
                    , typeExpr = In pairDuc
                    , kind = Nothing }
          , Expression (Constructor pairDuc 0 :@: UnitExpr :@: UnitExpr)])
    let nat = [ "data Nat : Set where"
              , "  Z : Unit -> Nat"
              , "  S : Nat -> Nat"]
        natDuc = Ductive { gamma = []
                         , sigmas = [[], []]
                         , as = [ UnitType,  LocalTypeVar 0 "Nat"]
                         , gamma1s = [ [], [] ]
                         , nameDuc = "Nat"
                         , strNames = ["Z", "S"]}
        natRes = TypeDef { name = "Nat"
                         , parameterCtx = []
                         , typeExpr = In natDuc
                         , kind = Nothing }
    it "parse pairs of nats" $
       parse parseProgram "" (T.unlines $ nat <>
                                        [ "one = S @ (Z @ ())"
                                        , "data Pair : Set where"
                                        , "  MkPair : (x:Nat) -> Unit -> Pair"
                                        , "MkPair @ one @ one"
                                        ])
      `shouldParse`
      (let pairDuc = Ductive { gamma = []
                             , sigmas = [[]]
                             , as = [ UnitType ]
                             , gamma1s = [ [ GlobalTypeVar "Nat" [] ] ]
                             , nameDuc = "Pair"
                             , strNames = ["MkPair"]}
       in [ natRes
          , ExprDef { name = "one"
                    , expr = Constructor natDuc 1
                             :@: (Constructor natDuc 0
                                  :@: UnitExpr)
                    , ty = Nothing }
          , TypeDef { name = "Pair"
                    , parameterCtx = []
                    , typeExpr = In pairDuc
                    , kind = Nothing }
          , Expression (Constructor pairDuc 0 :@: GlobalExprVar "one" :@: GlobalExprVar "one")])
    let copair = [ "codata Copair<A : Set,B : Set> : Set where"
                 , "  First : Copair -> A"
                 , "  Second : Copair -> B" ]
        copairDuc = Ductive { gamma = []
                             , sigmas = [[], []]
                             , as = [ Parameter 1 "A", Parameter 0 "B" ]
                             , gamma1s = [ [], [] ]
                             , nameDuc = "Copair"
                             , strNames = ["First", "Second"]}
        copairRes = TypeDef { name = "Copair"
                            , parameterCtx = [[],[]]
                            , typeExpr = Coin copairDuc
                            , kind = Nothing }
    it "parse List of Units" $
       parse parseProgram "" (T.unlines $ copair
                                         <> [ "data ListUnit : Set where"
                                            , "  LNilU : Unit -> ListUnit"
                                            , "  LConsU : Copair<Unit, ListUnit> -> ListUnit"
                                            , "LConsU @ (( corec<Unit,ListUnit> Unit to Copair where"
                                            , "             { First x = ()"
                                            , "             ; Second x = LNilU @ () }) @ ())"
                                            ])
      `shouldParse`
      (let listUnitDuc = Ductive { gamma = []
                                 , sigmas = [[], []]
                                 , as = [ UnitType
                                        , GlobalTypeVar "Copair" [ UnitType
                                                                 , LocalTypeVar 0 "ListUnit"] ]
                                 , gamma1s = [ [], [] ]
                                 , nameDuc = "ListUnit"
                                 , strNames = ["LNilU", "LConsU"]}
       in [ copairRes
          , TypeDef { name = "ListUnit"
                    , parameterCtx = []
                    , typeExpr = In listUnitDuc
                    , kind = Nothing }
          , Expression $ Constructor listUnitDuc 1
                         :@: (WithParameters [UnitType, GlobalTypeVar "ListUnit" []]
                                            Corec { fromCorec = UnitType
                                                  , toCorec = copairDuc
                                                  , matches = [ UnitExpr
                                                              , Constructor listUnitDuc 0
                                                                :@: UnitExpr ]
                                                  } :@: UnitExpr)])

    it "parse Vec of Units" $
       parse parseProgram "" (T.unlines $ copair <> nat
                                          <> [ "data Vec : (n : Nat) -> Set where"
                                             , "  Nil : Unit -> Vec (Z @ ())"
                                             , "  Cons : (n : Nat) -> Copair<Unit,Vec @ n> -> Vec (S @ n)"])
       `shouldParse`
       (let vecDuc = Ductive { gamma = [GlobalTypeVar "Nat" []]
                             , sigmas = [ [Constructor natDuc 0 :@: UnitExpr]
                                        , [Constructor natDuc 1 :@: LocalExprVar 0 "n"]]
                             , as = [ UnitType
                                    , GlobalTypeVar "Copair" [ UnitType
                                                             , LocalTypeVar 0 "Vec"
                                                               :@ LocalExprVar 0 "n"] ]
                             , gamma1s = [ [], [GlobalTypeVar "Nat" []] ]
                             , nameDuc = "Vec"
                             , strNames = ["Nil", "Cons"]}
       in [ copairRes
          , natRes
          , TypeDef { name = "Vec"
                    , parameterCtx = []
                    , typeExpr = In vecDuc
                    , kind = Nothing }])

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
                    , expr = UnitExpr
                    , ty = Nothing}
    it "inlines Global var with unit expr" $
      shouldCheckInGlobCtx [a]
                           (inlineExpr $ GlobalExprVar "a")
                           UnitExpr

  describe "Type Checker works" $ do
    let shouldCheck :: (HasCallStack, Show a, Eq a) => TI ann a -> a -> Expectation
        shouldCheck ti = shouldCheckIn ti emptyCtx
    it "type check unit type" $
      inferType UnitType
      `shouldCheck`
      []
    it "type check unit expr" $
      inferTerm UnitExpr
      `shouldCheck`
      ([],UnitType)

    -- natural numbers
    let natDuc = Ductive { gamma = []
                         , sigmas = [[],[]]
                         , as = [UnitType, LocalTypeVar 0 "X"]
                         , gamma1s = [[],[]]
                         , nameDuc = "nat"
                         , strNames = ["S", "N"]}
        nat = In natDuc
        idNat = Rec { fromRec = natDuc
                    , toRec = nat
                    , matches = [ Constructor natDuc 0
                                  :@: LocalExprVar 0 "y1"
                                , Constructor natDuc 1
                                  :@: LocalExprVar 0 "y2"]
                    }
        zero = Constructor natDuc 0:@: UnitExpr
        suc = Constructor natDuc 1
        one = suc :@: zero
        two = suc :@: one
        three = suc :@: two
        four = suc :@: three
        five = suc :@: four
        six = suc :@: five
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
      typeAction UnitType
                 [idNat :@: LocalExprVar 0 ""]
                 [[]] [UnitType] [nat]
      `shouldCheck`
      LocalExprVar 0 ""
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
      typeAction (LocalTypeVar 0 "")
                 [idNat :@: LocalExprVar 0 ""]
                 [[]] [nat] [nat]
      `shouldCheck`
      (idNat :@: LocalExprVar 0 "")
    it "substExpr works" $
      substExpr 0 (idNat :@: LocalExprVar 0 "")
                  (Constructor natDuc 1
                   :@: LocalExprVar 0 "")
      `shouldBe`
      Constructor natDuc 1
      :@: (idNat :@: LocalExprVar 0 "")
    it "substExprs works" $
      substExprs 0 [zero] (Constructor natDuc 1
                           :@: (idNat :@: LocalExprVar 0 ""))
      `shouldBe`
      (Constructor natDuc 1 :@: (idNat :@: zero))
    it "evaluates the identity function on one to one" $
      evalExpr (idNat :@: one)
      `shouldCheck`
      one

    -- inductive pairs
    let ducPair x y = Ductive { gamma = []
                              , sigmas = [[]]
                              , as = [y]
                              , gamma1s = [[x]]
                              , nameDuc = pShow x <> " x " <> pShow y
                              , strNames = ["Fst", "Snd"]
                              }
        pair x y = In $ ducPair x y
        mkPair x y = Constructor (ducPair x y) 0
        fst x y = Rec { fromRec = ducPair x y
                      , toRec = x
                      , matches = [LocalExprVar 1 ""]}
        snd x y = Rec { fromRec = ducPair x y
                      , toRec = y
                      , matches = [LocalExprVar 0 ""]}
    it "type checks fst on nat and unit tor (nat x unit) -> nat" $
      inferTerm (fst nat UnitType)
      `shouldCheck`
      ([pair nat UnitType],nat)
    it "type checks fst on pair of one and two to nat" $
      inferTerm (fst nat nat :@: (mkPair nat nat :@: one :@: two))
      `shouldCheck`
      ([],nat)
    it "evals fst on pair of unit and unit to unit" $
      evalExpr (fst UnitType UnitType :@: (mkPair UnitType UnitType :@: UnitExpr :@: UnitExpr))
      `shouldCheck`
      UnitExpr
    it "type checks fst on pair of unit and two to unit" $
      inferTerm (fst UnitType nat :@: (mkPair UnitType nat :@: UnitExpr :@: two))
      `shouldCheck`
      ([],UnitType)
    it "evals fst on pair of unit and two to unit" $
      evalExpr (fst UnitType nat :@: (mkPair UnitType nat :@: UnitExpr :@: two))
      `shouldCheck`
      UnitExpr
    it "evals fst on pair of zero and one to zero" $
      evalExpr (fst nat nat :@: (mkPair nat nat :@: zero :@: one))
      `shouldCheck`
      zero
    it "evals fst on pair of one and one to one" $
      evalExpr (fst nat nat :@: (mkPair nat nat :@: one :@: one))
      `shouldCheck`
      one
    it "evals fst on pair of one and two to one" $
      evalExpr (fst nat nat :@: (mkPair nat nat :@: one :@: two))
      `shouldCheck`
      one
    it "type checks snd on pair of one and two to nat" $
      inferTerm (snd nat nat :@: (mkPair nat nat :@: one :@: two))
      `shouldCheck`
      ([],nat)
    it "evals snd on pair of one and two to two" $
      evalExpr (snd nat nat :@: (mkPair nat nat :@: one :@: two))
      `shouldCheck`
      two

    -- addition
    let add =  Rec { fromRec = ducPair nat nat
                   , toRec = nat
                   , matches = [Rec { fromRec = natDuc
                                    , toRec = nat
                                    , matches = [ LocalExprVar 1 ""
                                                , suc :@: LocalExprVar 0 "" ]} :@: LocalExprVar 1 ""]}
    it "type check add to (nat x nat) -> nat" $
      inferTerm add
      `shouldCheck`
      ([pair nat nat], nat)
    it "type action on nat give back idNat applied to variable" $
      typeAction nat [add :@: LocalExprVar 0 ""] [[nat]] [nat] [nat]
      `shouldCheck`
      (idNat :@: LocalExprVar 0 "")
    it "type action on nat give back idNat applied to variable doesn't change in substitution" $
      (flip (substExpr 0)
       ( Rec { fromRec = natDuc
             , toRec = nat
             , matches = [ LocalExprVar 1 ""
                         , suc :@: LocalExprVar 0 "" ]} :@: LocalExprVar 1 "" )
        <$> typeAction nat [add :@: LocalExprVar 0 ""] [[nat]] [nat] [nat])
      `shouldCheck`
       ( Rec { fromRec = natDuc
            , toRec = nat
            , matches = [ idNat :@: LocalExprVar 1 ""
                        , suc :@: LocalExprVar 0 "" ]} :@: LocalExprVar 1 "" )
    it "add zero and zero to zero" $
      evalExpr (add :@: (mkPair nat nat :@: zero :@: zero))
      `shouldCheck`
      zero
    it "add two and three to five" $
      evalExpr (add :@: (mkPair nat nat :@: two :@: three))
      `shouldCheck`
      five

    -- packed units
    let ducPacked x = Ductive { gamma = []
                              , sigmas = [[]]
                              , as = [x]
                              , gamma1s = [[]]
                              , nameDuc = "packed " <> pShow x
                              , strNames = ["Pac"]
                              }
        packed x = In $ ducPacked x
        ducPackedUnit = ducPacked UnitType
        packedUnit = packed UnitType
        unpack x = Rec { fromRec = ducPacked x
                       , toRec = x
                       , matches = [LocalExprVar 0 ""]}
        unpackUnit = unpack UnitType
        pack x = Constructor (ducPacked x) 0
        pu = pack UnitType :@: UnitExpr
        ducPPUnit = ducPacked packedUnit
        ppUnit = In ducPPUnit
        unppUnit =  Rec { fromRec = ducPPUnit
                        , toRec = UnitType
                        , matches = [unpackUnit :@: LocalExprVar 0 ""]}
        ppu = pack packedUnit :@: pu
    it "type checks unpackUnit function on pu to the unit expression" $
      inferTerm (unpackUnit :@: pu)
      `shouldCheck`
      ([],UnitType)
    it "evaluates the unpackUnit function on pu to the unit expression" $
      evalExpr (unpackUnit :@: pu)
      `shouldCheck`
      UnitExpr
    it "type checks unppUnit function on ppu to the unit expression" $
      inferTerm (unppUnit :@: ppu)
      `shouldCheck`
      ([],UnitType)
    it "evaluates the unppUnit function on ppu to the unit expression" $
      evalExpr (unppUnit :@: ppu)
      `shouldCheck`
      UnitExpr

    -- packed nats
    let pNatDuc = Ductive { gamma = []
                          , sigmas = [[],[]]
                          , as = [UnitType, In $ ducPacked $ LocalTypeVar 1 "X"]
                          , gamma1s = [[],[]]
                          , nameDuc = "pNat"
                          , strNames = ["Z", "S"]}
        pNat = In pNatDuc
        idpNat = Rec { fromRec = pNatDuc
                     , toRec = pNat
                     , matches = [ Constructor pNatDuc 0
                                  :@: LocalExprVar 0 "y1"
                                , Constructor pNatDuc 1
                                  :@: LocalExprVar 0 "y2"]
                    }
        unpackIdpNat = Rec { fromRec = ducPacked pNat
                           , toRec = In $ ducPacked pNat
                           , matches = [ pack pNat :@: (idpNat :@: LocalExprVar 0 "")]}
        pZero = Constructor pNatDuc 0:@: UnitExpr
        pOne = Constructor pNatDuc 1 :@: (pack pNat :@: pZero)
        pTwo = Constructor pNatDuc 1 :@: (pack pNat :@: pOne)
        pThree = Constructor pNatDuc 1 :@: (pack pNat :@: pTwo)
    it "evaluates the pNat identity function to the pNat identity function" $
      evalExpr idpNat
      `shouldCheck`
      idpNat
    it "evaluates the pZero to the pZero" $
      evalExpr pZero
      `shouldCheck`
      pZero
    it "typeaction works" $
      typeAction UnitType
                 [idpNat :@: LocalExprVar 0 ""]
                 [[]] [UnitType] [pNat]
      `shouldCheck`
      LocalExprVar 0 ""
    it "evaluates the identity function on pZero to pZero" $
      evalExpr (idpNat :@: pZero)
      `shouldCheck`
      pZero
    it "evaluates the pOne to the pOne" $
      evalExpr pOne
      `shouldCheck`
      pOne
    it "infer type of expr pZero yields Nat" $
      inferTerm pZero
      `shouldCheck`
      ([], pNat)
    it "infers type for succ constructor of pNat" $
      inferTerm (Constructor pNatDuc 1)
      `shouldCheck`
      ([In $ ducPacked pNat], pNat)
    it "infer type of expr pOne yields pNat" $
      inferTerm pOne
      `shouldCheck`
      ([], pNat)
    it "pNat: typeaction works on typeVar" $
      typeAction (LocalTypeVar 0 "")
                 [idpNat :@: LocalExprVar 0 ""]
                 [[]] [pNat] [pNat]
      `shouldCheck`
      (idpNat :@: LocalExprVar 0 "")
    it "pNat: substExpr works" $
      substExpr 0 (idpNat :@: LocalExprVar 0 "")
                  (Constructor pNatDuc 1
                   :@: LocalExprVar 0 "")
      `shouldBe`
      Constructor pNatDuc 1
      :@: (idpNat :@: LocalExprVar 0 "")
    it "substExprs works" $
      substExprs 0 [pZero] (Constructor pNatDuc 1
                           :@: (idpNat :@: LocalExprVar 0 ""))
      `shouldBe`
      (Constructor pNatDuc 1 :@: (idpNat :@: pZero))
    it "evaluates the identity function on pZero to pZero" $
      evalExpr (idpNat :@: pZero)
      `shouldCheck`
      pZero
    it "evaluates the identity function on pZero to pZero" $
      evalExpr (idpNat :@: pZero)
      `shouldCheck`
      pZero
    it "????" $
      evalExpr (unpackIdpNat :@: (pack pNat :@: pZero))
      `shouldCheck`
      (pack pNat :@: pZero)
    -- TODO fails in first typeAction case on second call
    it "evaluates the identity function on pOne to pOne" $
      evalExpr (idpNat :@: pOne)
      `shouldCheck`
      pOne

 -- copacked units
    let copackedUnit = Coin ducPackedUnit
        counpack x = Destructor (ducPacked x) 0
        copack x vx = Corec { fromCorec = UnitType
                            , toCorec = ducPacked x
                            , matches = [vx]} :@: UnitExpr
        counpackUnit = counpack UnitType
        cpu = copack UnitType UnitExpr
        coducPPUnit = ducPacked copackedUnit
        coppUnit = Coin coducPPUnit
        counppUnit x = counpackUnit :@: (counpack (Coin ducPackedUnit) :@: x)
        cppu = copack copackedUnit cpu
    it "type checks counpackUnit function" $
      inferTerm counpackUnit
      `shouldCheck`
      ([copackedUnit], UnitType)
    it "type checks counpackUnit function on cpu to the unit expression" $
      inferTerm (counpackUnit :@: cpu)
      `shouldCheck`
      ([],UnitType)
    it "evaluates the counpackUnit function on pu to the unit expression" $
      evalExpr (counpackUnit :@: cpu)
      `shouldCheck`
      UnitExpr
    it "type check counpack (Coin ducPackedUnit)" $
      inferTerm (counpack (Coin ducPackedUnit))
      `shouldCheck`
      ([Coin $ ducPacked copackedUnit], copackedUnit)
    it "type check cppu" $
      inferTerm cppu
      `shouldCheck`
      ([], Coin $ ducPacked copackedUnit)
    it "type check counpack (Coin ducPackedUnit) @ cppu" $
      inferTerm (counpack (Coin ducPackedUnit) :@: cppu)
      `shouldCheck`
      ([], copackedUnit)
    it "type checks counppUnit function on cppu to the unit type" $
      inferTerm (counppUnit cppu)
      `shouldCheck`
      ([],UnitType)
    it "evaluates the counppUnit function on cppu to the unit expression" $
      evalExpr (counppUnit cppu)
      `shouldCheck`
      UnitExpr

    -- copacked nats
    let cpNatDuc = Ductive { gamma = []
                           , sigmas = [[],[]]
                           , as = [UnitType, Coin $ ducPacked $ LocalTypeVar 1 "X"]
                           , gamma1s = [[],[]]
                           , nameDuc = "cpNat"
                           , strNames = ["Z", "S"]}
        cpNat = In cpNatDuc
        idcpNat = Rec { fromRec = cpNatDuc
                      , toRec = cpNat
                      , matches = [ Constructor cpNatDuc 0
                                   :@: LocalExprVar 0 "y1"
                                 , Constructor cpNatDuc 1
                                   :@: LocalExprVar 0 "y2"]
                      }
        {-
        unpackIdcpNat = Rec { fromRec = ducPacked pNat
                            , toRec = In $ ducPacked pNat
                            , matches = [ pack pNat :@: (idpNat :@: LocalExprVar 0 "")]}
        -}
        cpZero = Constructor cpNatDuc 0 :@: UnitExpr
        cpOne = Constructor cpNatDuc 1 :@: copack cpNat cpZero
        cpTwo = Constructor cpNatDuc 1 :@: copack cpNat cpOne
        cpThree = Constructor cpNatDuc 1 :@: copack cpNat cpTwo
    it "evaluates the cpNat identity function to the cpNat identity function" $
      evalExpr idcpNat
      `shouldCheck`
      idcpNat
    it "evaluates the pZero to the pZero" $
      evalExpr cpZero
      `shouldCheck`
      cpZero
    it "evaluates the identity function on cpZero to cpZero" $
      evalExpr (idcpNat :@: cpZero)
      `shouldCheck`
      cpZero
    it "evaluates the cpOne to the cpOne" $
      evalExpr cpOne
      `shouldCheck`
      cpOne
    it "infer type of expr cpZero yields cpNat" $
      inferTerm cpZero
      `shouldCheck`
      ([], cpNat)
    it "infers type for succ constructor of pNat" $
      inferTerm (Constructor pNatDuc 1)
      `shouldCheck`
      ([In $ ducPacked pNat], pNat)
    it "infer type of expr cpOne yields cpNat" $
      inferTerm cpOne
      `shouldCheck`
      ([], cpNat)
    it "type cechs the identity function on cpOne to cpNat" $
      inferTerm (idcpNat :@: cpOne)
      `shouldCheck`
      ([], cpNat)
    it "evaluation of the identity function on cpOne preserves type" $
      (evalExpr (idcpNat :@: cpOne) >>= inferTerm)
      `shouldCheck`
      ([], cpNat)


    -- bools
    let boolDuc = Ductive { gamma = []
                          , sigmas = [[],[]]
                          , as = [UnitType, UnitType]
                          , gamma1s = [[],[]]
                          , nameDuc = "bool"
                          , strNames = ["TT", "FF"]}
        bool = In boolDuc
        false = Constructor boolDuc 0
        true = Constructor boolDuc 1

    -- maybe
    let maybeDuc x = Ductive { gamma = []
                             , sigmas = [ [], []]
                             , as = [ UnitType, x]
                             , gamma1s = [[],[]]
                             , nameDuc = "Maybe " <> pShow x
                             , strNames = ["Nothing", "Just"]}
        maybe x = In $ maybeDuc x
        nothing x = Constructor (maybeDuc x) 0 :@: UnitExpr
        just x = Constructor (maybeDuc x) 1
        isNothing x = Rec { fromRec = maybeDuc x
                          , toRec = bool
                          , matches = [true, false]}
        isJust x = Rec { fromRec = maybeDuc x
                       , toRec = bool
                       , matches = [false, true]}
    it "infer type maybe nat for nothing nat" $
      inferTerm (nothing nat)
      `shouldCheck`
      ([], maybe nat)
    it "infer type maybe nat for just on one" $
      inferTerm (just nat :@: one)
      `shouldCheck`
      ([], maybe nat)
    -- copairs
    let copairDuc x y = Ductive { gamma = []
                                , sigmas = [ [] , []]
                                , as = [ x , y ]
                                , gamma1s = [[],[]]
                                , nameDuc = pShow x <> " x " <> pShow y
                                , strNames = ["Fst", "Snd"]}
        copair x y = Coin $ copairDuc x y
        mkCopair tyX tyY x y= Corec { fromCorec = UnitType
                                    , toCorec = copairDuc tyX tyY
                                    , matches = [x,y]} :@: UnitExpr
        cofst x y = Destructor (copairDuc x y) 0
        cosnd x y = Destructor (copairDuc x y) 1
    it "infer copair of units for copair of unit expression " $
      inferTerm (mkCopair UnitType UnitType UnitExpr UnitExpr)
      `shouldCheck`
      ([],copair UnitType UnitType)
    it "infer copair of nats for copair of one and two" $
      inferTerm (mkCopair nat nat one two)
      `shouldCheck`
      ([],copair nat nat)
    it "infer nat for fst of copair of one and two" $
      inferTerm (cofst nat nat :@: mkCopair nat nat one two)
      `shouldCheck`
      ([],nat)
    it "evals fst on copair of one and two to one" $
      evalExpr (cofst nat nat :@: mkCopair nat nat one two)
      `shouldCheck`
       one
    it "infer nat for snd of copair of one and two" $
      inferTerm (cosnd nat nat :@: mkCopair nat nat one two)
      `shouldCheck`
      ([],nat)
    it "evals snd on copair of one and two to two" $
      evalExpr (cosnd nat nat :@: mkCopair nat nat one two)
      `shouldCheck`
       two

    -- pair and copair are isomporph
    let pairToCopair x y = Rec { fromRec = ducPair x y
                               , toRec = copair x y
                               , matches = [mkCopair x y (LocalExprVar 0 "")
                                                         (LocalExprVar 1 "")]}
        copairToPair x y = Rec { fromRec = ducPacked (copair x y)
                               , toRec = pair x y
                               , matches = [mkPair x y
                                            :@: (cofst x y
                                                 :@: LocalExprVar 0 "")
                                            :@: (cosnd x y
                                                 :@: LocalExprVar 0 "")]}
    it "typechecks pairToCopair Unit Unit" $
      inferTerm (pairToCopair UnitType UnitType)
      `shouldCheck`
      ([pair UnitType UnitType], copair UnitType UnitType)
    it "typechecks pairToCopair Unit nat" $
      inferTerm (pairToCopair UnitType nat)
      `shouldCheck`
      ([pair UnitType nat], copair UnitType nat)
    it "evaluates fst pairToCopair Unit nat on unit and one" $
      evalExpr (cofst UnitType nat
                :@: (pairToCopair UnitType nat
                     :@: (mkPair UnitType UnitType :@: UnitExpr :@: one)))
      `shouldCheck`
      UnitExpr
    it "evaluates snd pairToCopair Unit nat on unit and one" $
      evalExpr (cosnd UnitType nat
                :@: (pairToCopair UnitType nat
                     :@: (mkPair UnitType UnitType :@: UnitExpr :@: one)))
      `shouldCheck`
      one
    it "type checks copairToPair Unit Unit" $
      inferTerm (copairToPair UnitType UnitType)
      `shouldCheck`
      ([packed (copair UnitType UnitType)], pair UnitType UnitType)
    it "type checks copairToPair Unit nat" $
      inferTerm (copairToPair UnitType nat)
      `shouldCheck`
      ([packed (copair UnitType nat)], pair UnitType nat)
    it "type checks copairToPair Unit nat" $
      evalExpr (copairToPair UnitType nat
                :@: (pack (copair UnitType nat)
                     :@: mkCopair UnitType nat UnitExpr one  ))
      `shouldCheck`
      (mkPair UnitType nat :@: UnitExpr :@: one)
    -- Lists with copairs
    let pairDucVar = Ductive { gamma = []
                             , sigmas = [[],[]]
                             , as = [UnitType, LocalTypeVar 1 "X"]
                             , gamma1s = [[],[]]
                             , nameDuc = ""
                             , strNames = ["Fst", "Snd"]}--Just "pair"}
        pairVar = Coin pairDucVar
        listDuc = Ductive { gamma = []
                          , sigmas = [[],[]]
                          , as = [ UnitType, pairVar]
                          , gamma1s = [[],[]]
                          , nameDuc = ""
                          , strNames = ["Nil", "Cons"]}--Just "list"}
        list = In listDuc
        pairDuc = Ductive { gamma = []
                          , sigmas = [[],[]]
                          , as = [UnitType, list]
                          , gamma1s = [[],[]]
                          , nameDuc = ""
                          , strNames = ["Fst", "Snd"]}--Just "pair"}
        pair = Coin pairDuc
        idList = Rec { fromRec = listDuc
                     , toRec = list
                     , matches = [ Constructor listDuc 0
                                   :@: LocalExprVar 0 "y1"
                                 , Constructor listDuc 1
                                   :@: LocalExprVar 0 "y2"]
                     }
        emptyList = Constructor listDuc 0 :@: UnitExpr
        mkPair x y = Corec { fromCorec = UnitType
                           , toCorec = pairDuc
                           , matches = [x,y]} :@: UnitExpr
        oneList = Constructor listDuc 1
                  :@: mkPair UnitExpr emptyList
    it "infers type list for emptyList" $
      inferTerm emptyList
      `shouldCheck`
      ([],list)
    it "infers type list for oneList" $
      inferTerm oneList
      `shouldCheck`
      ([],list)
    it "infers type list for identity function on oneList" $
      inferTerm (idList :@: oneList)
      `shouldCheck`
      ([],list)
    it "evaluation of the identity function on list preserves typing" $
      (evalExpr (idList :@: oneList) >>= inferTerm)
      `shouldCheck`
      ([],list)
    -- vector without pair typ, just save the element in gamma2
    let suc = Constructor natDuc 1
        vec2Duc = Ductive { gamma = [ nat ]
                          , sigmas = [ [zero]
                                     , [suc :@: LocalExprVar 2 "k"]]
                          , as = [ UnitType
                                 , LocalTypeVar 0 "X" :@ LocalExprVar 1 "k"]
                          , gamma1s = [[],[nat, UnitType]]
                          , nameDuc = "vec2"
                          , strNames = ["Nil", "Cons"]}

        vec2 = In vec2Duc
        emptyVec2 = Constructor vec2Duc 0 :@: UnitExpr
        oneVec2 = Constructor vec2Duc 1 :@: zero :@: UnitExpr
                 :@: emptyVec2
        idVec2 = Rec { fromRec = vec2Duc
                     , toRec = vec2
                     , matches = [ Constructor vec2Duc 0
                                   :@: LocalExprVar 0 "y1"
                                 , Constructor vec2Duc 1
                                   :@: LocalExprVar 2 "k"
                                   :@: LocalExprVar 1 "x"
                                   :@: LocalExprVar 0 "y2"]
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
