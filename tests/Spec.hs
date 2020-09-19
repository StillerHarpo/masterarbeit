{-# language OverloadedStrings#-}
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import           Control.Monad.State.Strict

import           Lens.Micro.Platform

import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range          as Range

import           Test.Hspec.Expectations
import           Test.Hspec.Megaparsec
import           Test.Hspec
import           Test.Hspec.Hedgehog                    ( (===)
                                                        , forAll
                                                        , hedgehog)

import           Text.Megaparsec


import           Parser
import           ShiftFreeVars
import           TypeChecker
import           TypeAction
import           Mark
import           Subst
import           Eval
import           Betaeq
import           AbstractSyntaxTree

import           Lib
import qualified Bool
import qualified Nat
import qualified Packed
import qualified Maybe
import qualified Pair
import qualified Conat
import qualified List
import qualified Stream
import qualified Vector

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
      [ TypeDef $ OpenDuctive {
                    gamma = [],
                    inOrCoin = IsIn,
                    parameterCtx = [],
                    strDefs = [StrDef {
                      sigma = [],
                      a = Abstr UnitType UnitType,
                      gamma1 = [],
                      strName = "C"}],
                    nameDuc = "A"}]
    it "parses a abstraction with multiple arguments" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       -- TODO this should work without brackets
                                       , "   C : ((x:Unit,y:Unit).Unit) -> A"])
      `shouldParse`
      [ TypeDef $ OpenDuctive {
                    gamma = [],
                    inOrCoin = IsIn,
                    parameterCtx = [],
                    strDefs = [StrDef {
                      sigma = [],
                      a = Abstr UnitType (Abstr UnitType UnitType),
                      gamma1 = [],
                      strName = "C"}],
                    nameDuc = "A"}]
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
    let inC = OpenDuctive { gamma = []
                          , inOrCoin = IsIn
                          , parameterCtx = []
                          , strDefs = [StrDef {
                                sigma = []
                              , a = LocalTypeVar 0 False "C"
                              , gamma1 = []
                              , strName = "C1"}]
                          , nameDuc = "C" }
        coinC = inC { inOrCoin = IsCoin }
    it "parses data" $
      parse parseProgram "" "data C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef inC]
    it "parses codata" $
      parse parseProgram "" "codata C : Set where { C1 : C -> C }"
      `shouldParse`
      [TypeDef coinC]
    -- TODO Tests for constructors and destructors
    it "parses rec" $
      parse parseProgram "" "data C : Set where { C1 : C -> C};rec C to Unit where { C1 x = () }"
      `shouldParse`
      [ TypeDef inC
      , Expression $ Iter inC [] UnitType [UnitExpr]]
    it "parses corec" $
      parse parseProgram "" "codata C : Set where { C1 : C -> C}; corec Unit to C where { C1 x = ()}"
      `shouldParse`
      [ TypeDef coinC
      , Expression $ Iter coinC [] UnitType [UnitExpr]]
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
      [TypeDef $ OpenDuctive {
                   gamma = [UnitType],
                   inOrCoin = IsIn,
                   parameterCtx = [],
                   strDefs = [ StrDef { sigma = [UnitExpr]
                                      , a = LocalTypeVar 0 False "C" :@ UnitExpr
                                      , gamma1 = []
                                      , strName = "C1"}
                             , StrDef { sigma = [UnitExpr]
                                      , a =  LocalTypeVar 0 False "C"
                                             :@ LocalExprVar 0 False "y"
                                      , gamma1 = [UnitType]
                                      , strName = "C2"}],
                   nameDuc = "C"}]
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
      , TypeDef $ OpenDuctive {
                    parameterCtx = [],
                    inOrCoin = IsIn,
                    gamma = [ UnitType],
                    strDefs = [ StrDef { sigma = [UnitExpr],
                                         a = LocalTypeVar 0 False "A":@ UnitExpr,
                                         gamma1 = [],
                                         strName = "C1"}
                              , StrDef { sigma = [UnitExpr],
                                         a = LocalTypeVar 0 False "A" :@ LocalExprVar 0 False "y",
                                         gamma1 = [UnitType],
                                         strName = "C2"}],
                    nameDuc = "A"}
      , Expression $ GlobalExprVar "x" [] [] :@: GlobalExprVar "z" [] []]
    it "parses a rec programm" $
      parse parseProgram "" (T.unlines [ "data A : Set where"
                                       , "  C : A -> A"
                                       , "rec A to A where"
                                       , "  C x = C @ x"
                                       ])
      `shouldParse`
      (let a = OpenDuctive { gamma = []
                           , inOrCoin = IsIn
                           , parameterCtx = []
                           , strDefs = [ StrDef { sigma = []
                                                , a = LocalTypeVar 0 False "A"
                                                , gamma1 = []
                                                , strName = "C"} ]
                           , nameDuc = "A" }
      in [ TypeDef a
         , Expression Iter { ductive = a
                           , parameters = []
                           , motive = GlobalTypeVar "A" []
                           , matches = [ Structor a [] 0
                                         :@: LocalExprVar 0 False "x" ]}])
    it "parses context with spaces" $
       parse parseProgram "" "data A : (x : Unit, y : Unit) -> Set where"
       `shouldParse`
       [ TypeDef $ OpenDuctive { parameterCtx = []
                               , inOrCoin = IsIn
                               , gamma = [UnitType, UnitType]
                               , strDefs = []
                               , nameDuc = "A"}]
    it "parses parameters of inductive type" $
      parse parseProgram "" (T.unlines [ "data A<B : Set> : Set where"
                                       , "  C : B -> A"
                                       , "C<Unit>@()"])
      `shouldParse`
      let duc = OpenDuctive { gamma = []
                            , inOrCoin = IsIn
                            , parameterCtx = [[]]
                            , strDefs = [ StrDef { sigma = []
                                                 , a = Parameter 0 False "B"
                                                 , gamma1 = []
                                                 , strName = "C"}]
                            , nameDuc = "A"}
      in [ TypeDef duc
         , Expression (Structor { ductive = duc
                                , parameters = [UnitType]
                                , num = 0}
                       :@: UnitExpr)]
    it "parses parameters of coinductive type" $
      parse parseProgram "" (T.unlines [ "codata A<B : Set> : Set where"
                                       , "  C : A -> B"
                                       , "corec<Unit> Unit to A where"
                                       , "  C x = ()"])
      `shouldParse`
      let duc = OpenDuctive { gamma = []
                            , inOrCoin = IsCoin
                            , parameterCtx = [[]]
                            , strDefs = [ StrDef { sigma = []
                                                 , a = Parameter 0 False "B"
                                                 , gamma1 = []
                                                 , strName = "C"}]
                            , nameDuc = "A" }
      in [ TypeDef duc
         , Expression (Iter { motive = UnitType
                            , parameters = [UnitType]
                            , ductive = duc
                            , matches = [UnitExpr]})]
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
      (let a = OpenDuctive { gamma = []
                           , inOrCoin = IsIn
                           , parameterCtx = []
                           , strDefs = [ StrDef { sigma = []
                                                , a =  LocalTypeVar 0 False "A"
                                                , gamma1 = []
                                                , strName = "C1"}
                                       , StrDef { sigma = []
                                                , a =  LocalTypeVar 0 False "A"
                                                , gamma1 =  [UnitType]
                                                , strName = "C2"}
                                       , StrDef { sigma = []
                                                , a =  LocalTypeVar 0 False "A"
                                                , gamma1 = [UnitType, UnitType]
                                                , strName = "C3"}
                                       , StrDef { sigma = []
                                                , a =  LocalTypeVar 0 False "A"
                                                , gamma1 = [UnitType, UnitType, UnitType]
                                                , strName = "C4"}]
                           , nameDuc = "A" }
      in [ TypeDef a
         , Expression $ Iter { ductive = a
                             , parameters = []
                             , motive = GlobalTypeVar "A" []
                             , matches = [ Structor a [] 0
                                           :@: LocalExprVar 0 False "x"
                                         , Structor a [] 1
                                           :@: LocalExprVar 1 False "x"
                                           :@: LocalExprVar 0 False "y"
                                         , Structor a [] 2
                                           :@: LocalExprVar 2 False "x"
                                           :@: LocalExprVar 1 False "y"
                                          :@: LocalExprVar 0 False "z"
                                         , Structor a [] 3
                                           :@: LocalExprVar 3 False "x"
                                           :@: LocalExprVar 2 False "y"
                                           :@: LocalExprVar 1 False "z"
                                           :@: LocalExprVar 0 False "u"]}])

  let emptyInDuc = OpenDuctive { gamma = []
                               , inOrCoin = IsIn
                               , parameterCtx = []
                               , strDefs = []
                               , nameDuc = ""}
      emptyCoinDuc = emptyInDuc { inOrCoin = IsCoin}

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

  describe "(un)marking works" $ do
    it "marks every variable" $
      markInExpr (LocalExprVar 0 False ""
                  :@: LocalExprVar 1 False ""
                  :@: LocalExprVar 3 False "")
      `shouldBe`
      (LocalExprVar 0 True "" :@: LocalExprVar 1 True "" :@: LocalExprVar 3 True "")
    it "unmarks every variable" $
      unmarkInExpr (LocalExprVar 0 True ""
                  :@: LocalExprVar 1 True ""
                  :@: LocalExprVar 3 True "")
      `shouldBe`
      (LocalExprVar 0 False "" :@: LocalExprVar 1 False "" :@: LocalExprVar 3 False "")

  describe "Substitution Works" $ do
    it "substitutes unit expression in expression" $
      substExpr 0 UnitExpr (LocalExprVar 0 False "")
      `shouldBe`
      UnitExpr
    it "substitutes variable in expression" $
      substExpr 0 (LocalExprVar 0 False "") (LocalExprVar 0 False "")
      `shouldBe`
      LocalExprVar 0 False ""
    it "substitutes variable in expression with offset 1" $
      substExpr 1 (LocalExprVar 0 False "") (LocalExprVar 1 False "")
      `shouldBe`
      LocalExprVar 0 False ""
    it "substitutes expression which is bound by abstraction" $
      substTypeExpr 0 (LocalExprVar 2 False "") (Abstr UnitType
                                                (UnitType
                                                 :@ (LocalExprVar 0 False ""
                                                     :@: LocalExprVar 1 False "")))
      `shouldBe`
      Abstr UnitType (UnitType :@ (LocalExprVar 0 False ""
                                   :@: LocalExprVar 3 False ""))
    it "substitutes expression which is bound by abstraction with offset 1" $
      substTypeExpr 1 (LocalExprVar 2 False "") (Abstr UnitType
                                                (UnitType
                                                 :@ (LocalExprVar 0 False ""
                                                     :@: LocalExprVar 2 False "")))
      `shouldBe`
      Abstr UnitType (UnitType :@ (LocalExprVar 0 False ""
                                   :@: LocalExprVar 3 False ""))

    it "substitutes multible expressions " $
       substExprs 0 [LocalExprVar 2 False ""
                    , LocalExprVar 3 False ""] (LocalExprVar 0 False ""
                                               :@: LocalExprVar 1 False "")
       `shouldBe`
       (LocalExprVar 2 False "" :@: LocalExprVar 3 False "")
    it "substitutes epressions in parallel" $
       substExprs 0 [LocalExprVar 2 False ""
                    , LocalExprVar 1 False ""
                    , LocalExprVar 0 False ""] (LocalExprVar 0 False ""
                                                :@: LocalExprVar 1 False ""
                                                :@: LocalExprVar 2 False "")
       `shouldBe`
       (LocalExprVar 2 False "" :@: LocalExprVar 1 False "" :@: LocalExprVar 0 False "")
    it "substitutes epressions in parallel 2" $
       substExprs 0 [LocalExprVar 0 False ""
                    , LocalExprVar 1 False ""
                    , LocalExprVar 2 False ""] (LocalExprVar 0 False ""
                                                :@: LocalExprVar 1 False ""
                                                :@: LocalExprVar 2 False "")
       `shouldBe`
       (LocalExprVar 0 False "" :@: LocalExprVar 1 False "" :@: LocalExprVar 2 False "")
    let ducBound = emptyInDuc {strDefs = [StrDef { gamma1 = [UnitType, UnitType]
                                                 , a  = UnitType
                                                 , sigma = []
                                                 , strName = ""}]}
        iterBound = Iter { ductive = ducBound
                         , parameters = []
                         , motive = UnitType
                         , matches = []}
    it "substitute expression which is bound by Iteration" $
      substExpr 0 (LocalExprVar 2 False "")
                  (iterBound {matches = [LocalExprVar 1 False ""
                                         :@: LocalExprVar 3 False ""]})
      `shouldBe`
      iterBound {matches = [LocalExprVar 1 False "" :@: LocalExprVar 5 False ""]}
    it "substitute expression which is bound by Iteration with offset one" $
      substExpr 1 (LocalExprVar 3 False "")
                  (iterBound {matches = [LocalExprVar 4 False ""
                                         :@: LocalExprVar 5 False ""]})
      `shouldBe`
      iterBound {matches = [LocalExprVar 6 False "" :@: LocalExprVar 5 False ""]}

    it ("substitute multible expressions in expression which is"
        <> "bound by Iteration") $
      substExprs 0 [LocalExprVar 2 False "", LocalExprVar 3 False ""]
                  (iterBound {matches = [LocalExprVar 4 False ""
                                         :@: LocalExprVar 3 False ""]})
      `shouldBe`
      iterBound {matches = [LocalExprVar 6 False "" :@: LocalExprVar 5 False ""]}
    it ("substitute multible expressions in expression which is"
        <> "bound by Iteration 2") $
      substExprs 0 [LocalExprVar 2 False "", LocalExprVar 3 False ""]
                  (iterBound {matches = [ LocalExprVar 4 False ""
                                          :@: LocalExprVar 2 False ""]}
                   :@: LocalExprVar 0 False "")
      `shouldBe`
      (iterBound {matches = [LocalExprVar 6 False "" :@: LocalExprVar 2 False ""]}
       :@: LocalExprVar 2 False "")
    it "substitute type expression" $
      substType 0 UnitType (LocalTypeVar 0 False "")
      `shouldBe`
      UnitType
    let emptyStrDef = StrDef { gamma1 = []
                             , a = UnitType
                             , sigma = []
                             , strName = ""}
        emptyDuc = Ductive { openDuctive = emptyInDuc
                           , parametersTyExpr = []}
    it "substitutes a bound type expression" $
      substType 0 (LocalTypeVar 2 False "")
                  (emptyDuc {openDuctive = emptyInDuc { strDefs = [ emptyStrDef {a = LocalTypeVar 0 False ""}
                                                      , emptyStrDef {a = LocalTypeVar 1 False ""}]}})
      `shouldBe`
      emptyDuc {openDuctive = emptyInDuc {strDefs = [ emptyStrDef {a = LocalTypeVar 0 False ""}
                                                    , emptyStrDef {a = LocalTypeVar 3 False ""}]}}
    it "substitutes Parameter" $
      substPar 0 UnitType (Parameter 0 False "")
      `shouldBe`
      UnitType
    it "substitutes Parameters from over function" $
      fTyExpr (substParFuns 0 UnitType) (Parameter 0 False "")
      `shouldBe`
      UnitType
    it "substitutes Parameter in Ctx" $
      substParInCtx 0 UnitType [Parameter 0 False "", Parameter 1 False ""]
      `shouldBe`
      [UnitType, Parameter 1 False ""]
    it "substitue parameters in global variable" $
      substPar 0 UnitType (GlobalTypeVar "" [Parameter 0 False ""])
      `shouldBe`
      GlobalTypeVar "" [UnitType]
    it "substitue parameter in openDuctive" $
      substOpenDuctivePar 0 UnitType
        (OpenDuctive { gamma = [Parameter 0 False "x"]
                     , parameterCtx = []
                     , inOrCoin = IsIn
                     , strDefs = []
                     , nameDuc = ""})
      `shouldBe`
      (OpenDuctive { gamma = [UnitType]
                   , parameterCtx = []
                   , inOrCoin = IsIn
                   , strDefs = []
                   , nameDuc = ""})
    let ducWith x =
          Ductive {
              openDuctive =
                OpenDuctive { gamma = [x]
                            , parameterCtx = []
                            , inOrCoin = IsIn
                            , strDefs = [StrDef { sigma = []
                                                , a     = x
                                                , gamma1 = [x]
                                                , strName = ""}]
                            , nameDuc = ""}
            , parametersTyExpr = []}
    it "substitute Parameter with Unit in ductive" $
       substPar 0 UnitType (ducWith (Parameter 0 False ""))
       `shouldBe`
       ducWith UnitType
    it "substitute Parameter in parameters of type" $
       substPar 0 UnitType (emptyDuc {parametersTyExpr = [Parameter 0 False ""]})
       `shouldBe`
       emptyDuc {parametersTyExpr = [UnitType]}

  let shouldEval :: (HasCallStack, Show a, Eq a) => Eval ann a -> a -> Expectation
      shouldEval eval = shouldEvalIn eval emptyEvalCtx

  describe "Type Action works" $ do
    let pairDuc = OpenDuctive { nameDuc = "Pair"
                              , inOrCoin = IsCoin
                              , parameterCtx = [[],[]]
                              , gamma = []
                              , strDefs = [ StrDef { strName = "Fst"
                                                   , gamma1 = []
                                                   , sigma = []
                                                   , a = Parameter 1 False "A"}
                                          , StrDef { strName = "Fst"
                                                   , gamma1 = []
                                                   , sigma = []
                                                   , a = Parameter 0 False "B"}]}
        pairUnits = Ductive { openDuctive = pairDuc
                            , parametersTyExpr = [UnitType,UnitType]}
    it "Type action on mu with parameters works" $
      typeAction pairUnits [UnitExpr] [[]] [UnitType] [UnitType]
      `shouldEval`
      (Iter { ductive = pairDuc
            , parameters = [UnitType, UnitType]
            , motive = pairUnits
            , matches = [ Structor { ductive = pairDuc
                                   , parameters = [UnitType, UnitType]
                                   , num = 0} :@: LocalExprVar 0 False ""
                        , Structor { ductive = pairDuc
                                   , parameters = [UnitType, UnitType]
                                   , num = 1} :@: LocalExprVar 0 False "" ]}
      :@: LocalExprVar 0 False "")

  describe "Eval works" $ do
    it "evals unit type to unit type" $
      evalTypeExpr UnitType
      `shouldEval`
       UnitType
    it "evals abstraction to abstraction" $
      evalTypeExpr (Abstr UnitType UnitType)
      `shouldEval`
      Abstr UnitType UnitType
    it "evals \\T -> T @ () to T" $
      evalTypeExpr (Abstr UnitType UnitType :@ UnitExpr)
      `shouldEval`
      UnitType
    it "evals mu to mu" $
      evalTypeExpr (Ductive emptyInDuc [])
      `shouldEval`
      Ductive emptyInDuc []
    it "evals nu to nu" $
      evalTypeExpr (Ductive emptyCoinDuc [])
      `shouldEval`
      Ductive emptyCoinDuc []
    it "evals unit expression to Unit expression" $
      evalExpr UnitExpr
      `shouldEval`
      UnitExpr
    it "evals rec to rec" $
      evalExpr (Iter { ductive = emptyInDuc
                     , parameters = []
                     , motive = UnitType
                     , matches = []})
      `shouldEval`
      Iter { ductive = emptyInDuc
           , parameters = []
           , motive = UnitType
           , matches = []}
    it "evals corec to corec" $
      evalExpr (Iter { motive = UnitType
                     , ductive = emptyCoinDuc
                     , parameters = []
                     , matches = []})
      `shouldEval`
      Iter { motive = UnitType
           , ductive = emptyCoinDuc
           , parameters = []
           , matches = []}

  describe "Inlining works" $ do
    let ducA = emptyInDuc {nameDuc = "A"}
        tyA = TypeDef ducA
    it "inlines Global Var" $
      shouldEvalInGlobCtx [tyA]
                          (inlineTypeExpr $ GlobalTypeVar "A" [])
                          (Ductive (emptyInDuc {nameDuc = "A"}) [])
    let ducB = emptyInDuc { gamma = [GlobalTypeVar "A" []]
                          , nameDuc = "B"}
        tyB = TypeDef ducB
    it "inlines nested Global Var" $
      shouldEvalInGlobCtx [tyA,tyB]
                          (inlineTypeExpr $ GlobalTypeVar "B" [])
                          (Ductive (ducB {gamma = [Ductive ducA []]}) [])
    let ducC = emptyInDuc { gamma = [Parameter 0 False "x"]
                          , nameDuc = "C"}
        tyC = TypeDef ducC
    it "inlines parameterized Global Var" $
      shouldEvalInGlobCtx [tyC]
                          (inlineTypeExpr $ GlobalTypeVar "C" [UnitType])
                          (Ductive ducC [UnitType])
    it "inlines parameterized Global Var with Global Var as parameter" $
      shouldEvalInGlobCtx
        [tyC]
        (inlineTypeExpr $ GlobalTypeVar "C"
                                        [GlobalTypeVar "C" [UnitType]])
        (Ductive ducC [Ductive ducC [UnitType]])
    let a = ExprDef { name = "a"
                    , tyParameterCtx = []
                    , exprParameterCtx = []
                    , expr = UnitExpr
                    , ty = Nothing}
    it "inlines Global var with unit expr" $
      shouldEvalInGlobCtx [a]
                          (inlineExpr $ GlobalExprVar "a" [] [])
                          UnitExpr

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
      inferType (Ductive emptyInDuc [])
      `shouldCheck`
      []
    it "type checks nu" $
      inferType (Ductive emptyCoinDuc [])
      `shouldCheck`
      []
    it "type checks unit expr" $
      inferTerm UnitExpr
      `shouldCheck`
      ([],UnitType)
    it "type checks rec" $
      inferTerm (Iter { ductive = emptyInDuc
                      , parameters = []
                      , motive = UnitType
                      , matches = []})
      `shouldCheck`
      ([Ductive emptyInDuc []],UnitType)
    it "type checks corec" $
      inferTerm (Iter { motive = UnitType
                      , parameters = []
                      , ductive = emptyCoinDuc
                      , matches = []})
      `shouldCheck`
      ([UnitType], Ductive emptyCoinDuc [])

  -- Complete programs
  Bool.tests
  Packed.tests
  Maybe.tests
  Pair.tests
  Nat.tests
  Conat.tests
  List.tests
  Stream.tests
  Vector.tests
