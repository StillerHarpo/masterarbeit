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
                    nameDuc = Nothing}
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
                   as = [ LocalTypeVar 0 (Just "C")
                          :@ LocalExprVar 0 (Just "y")
                        , LocalTypeVar 0 (Just "C") :@ UnitExpr ],
                   gamma1s = [ [UnitType]
                             , []],
                   nameDuc = Nothing}
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
                    as = [ LocalTypeVar 0 (Just "A") :@ LocalExprVar 0 (Just "y")
                         , LocalTypeVar 0 (Just "A"):@ UnitExpr],
                    gamma1s = [ [UnitType]
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
                   , parameterCtx = []
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A" []
                          , matches = [ Constructor a 0 Nothing
                                        :@: LocalExprVar 0 (Just "x") ]}])
    it "parses context with spaces" $
       parse parseProgram "" "data A : (x : Unit, y : Unit) -> Set where"
       `shouldParse`
       [ TypeDef { name = "A"
                 , parameterCtx = []
                 , typeExpr = In Ductive { gamma = [UnitType, UnitType]
                                         , sigmas = []
                                         , as = []
                                         , gamma1s = []
                                         , nameDuc = Nothing}
                 , kind = Nothing }]
    it "parses parameters of inducive type" $
      parse parseProgram "" (T.unlines [ "data A<B : Set> : Set where"
                                       , "  C : B -> A"
                                       , "C<Unit>@()"])
      `shouldParse`
      let duc = Ductive { gamma = []
                        , sigmas = [[]]
                        , as = [LocalTypeVar 0 (Just "B")]
                        , gamma1s = [[]]
                        , nameDuc = Nothing}
      in [ TypeDef { name = "A"
                   , parameterCtx = [[]]
                   , typeExpr = In duc
                   , kind = Nothing}
         , Expression (WithParameters [UnitType] (Constructor { ductive = duc
                                                              , num = 0
                                                              , nameStr = Nothing})
                       :@: UnitExpr)]
    it "parses parameters of coinducive type" $
      parse parseProgram "" (T.unlines [ "codata A<B : Set> : Set where"
                                       , "  C : A -> B"
                                       , "corec<Unit> Unit to A where"
                                       , "  C x = ()"])
      `shouldParse`
      let duc = Ductive { gamma = []
                        , sigmas = [[]]
                        , as = [LocalTypeVar 0 (Just "B")]
                        , gamma1s = [[]]
                        , nameDuc = Just "A"}
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
                   , parameterCtx = []
                   , typeExpr = In a
                   , kind = Nothing}
         , Expression Rec { fromRec = a
                          , toRec = GlobalTypeVar "A" []
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

    -- natural numbers
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
        suc = Constructor natDuc 1 (Just "S")
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
      typeAction (LocalTypeVar 0 Nothing)
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

    -- inductive pairs
    let ducPair x y = Ductive { gamma = []
                              , sigmas = [[]]
                              , as = [y]
                              , gamma1s = [[x]]
                              , nameDuc = Just $ pShow x <> " x " <> pShow y
                              }
        pair x y = In $ ducPair x y
        mkPair x y = Constructor (ducPair x y) 0 (Just "mkPair")
        fst x y = Rec { fromRec = ducPair x y
                      , toRec = x
                      , matches = [LocalExprVar 1 Nothing]}
        snd x y = Rec { fromRec = ducPair x y
                      , toRec = y
                      , matches = [LocalExprVar 0 Nothing]}
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
                                    , matches = [ LocalExprVar 1 Nothing
                                                , suc :@: LocalExprVar 0 Nothing ]} :@: LocalExprVar 1 Nothing]}
    it "type check add to (nat x nat) -> nat" $
      inferTerm add
      `shouldCheck`
      ([pair nat nat], nat)
    it "type action on nat give back idNat applied to variable" $
      typeAction nat [add :@: LocalExprVar 0 Nothing] [[nat]] [nat] [nat]
      `shouldBe`
      idNat :@: LocalExprVar 0 Nothing
    it "type action on nat give back idNat applied to variable doesn't change in substitution" $
      substExpr 0
       (typeAction nat [add :@: LocalExprVar 0 Nothing] [[nat]] [nat] [nat])
       ( Rec { fromRec = natDuc
            , toRec = nat
            , matches = [ LocalExprVar 1 Nothing
                        , suc :@: LocalExprVar 0 Nothing ]} :@: LocalExprVar 1 Nothing )
      `shouldBe`
       ( Rec { fromRec = natDuc
            , toRec = nat
            , matches = [ idNat :@: LocalExprVar 1 Nothing
                        , suc :@: LocalExprVar 0 Nothing ]} :@: LocalExprVar 1 Nothing )
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
                              , nameDuc = Nothing
                              }
        ducPackedUnit = ducPacked UnitType
        packedUnit = In ducPackedUnit
        unpack x = Rec { fromRec = ducPacked x
                       , toRec = x
                       , matches = [LocalExprVar 0 Nothing]}
        unpackUnit = unpack UnitType
        pack x = Constructor (ducPacked x) 0 Nothing
        pu = pack UnitType :@: UnitExpr
        ducPPUnit = ducPacked packedUnit
        ppUnit = In ducPPUnit
        unppUnit =  Rec { fromRec = ducPPUnit
                        , toRec = UnitType
                        , matches = [unpackUnit :@: LocalExprVar 0 Nothing]}
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
                          , as = [UnitType, In $ ducPacked $ LocalTypeVar 1 (Just "X")]
                          , gamma1s = [[],[]]
                          , nameDuc = Just "pNat"}
        pNat = In pNatDuc
        idpNat = Rec { fromRec = pNatDuc
                     , toRec = pNat
                     , matches = [ Constructor pNatDuc 0 (Just "Z")
                                  :@: LocalExprVar 0 (Just "y1")
                                , Constructor pNatDuc 1 (Just "S")
                                  :@: LocalExprVar 0 (Just "y2")]
                    }
        unpackIdpNat = Rec { fromRec = ducPacked pNat
                           , toRec = In $ ducPacked pNat
                           , matches = [ pack pNat :@: (idpNat :@: LocalExprVar 0 Nothing)]}
        pZero = Constructor pNatDuc 0 (Just "Z"):@: UnitExpr
        pOne = Constructor pNatDuc 1 (Just "S") :@: (pack pNat :@: pZero)
        pTwo = Constructor pNatDuc 1 (Just "S") :@: (pack pNat :@: pOne)
        pThree = Constructor pNatDuc 1 (Just "S") :@: (pack pNat :@: pTwo)
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
                 [idpNat :@: LocalExprVar 0 Nothing]
                 [[]] [UnitType] [pNat]
      `shouldBe`
      LocalExprVar 0 Nothing
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
      inferTerm (Constructor pNatDuc 1 (Just "S"))
      `shouldCheck`
      ([In $ ducPacked pNat], pNat)
    it "infer type of expr pOne yields pNat" $
      inferTerm pOne
      `shouldCheck`
      ([], pNat)
    it "pNat: typeaction works on typeVar" $
      typeAction (LocalTypeVar 0 Nothing)
                 [idpNat :@: LocalExprVar 0 Nothing]
                 [[]] [pNat] [pNat]
      `shouldBe`
      idpNat :@: LocalExprVar 0 Nothing
    it "pNat: substExpr works" $
      substExpr 0 (idpNat :@: LocalExprVar 0 Nothing)
                  (Constructor pNatDuc 1 (Just "S")
                   :@: LocalExprVar 0 Nothing)
      `shouldBe`
      Constructor pNatDuc 1 (Just "S")
      :@: (idpNat :@: LocalExprVar 0 Nothing)
    it "substExprs works" $
      substExprs 0 [pZero] (Constructor pNatDuc 1 (Just "S")
                           :@: (idpNat :@: LocalExprVar 0 Nothing))
      `shouldBe`
      (Constructor pNatDuc 1 (Just "S") :@: (idpNat :@: pZero))
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
        counpack x = Destructor (ducPacked x) 0 Nothing
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
