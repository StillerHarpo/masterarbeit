{-# language OverloadedStrings#-}
module Nat.Functions where

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    (forAll, hedgehog)

import qualified Data.Text              as T
import           Data.Text              (Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Packed.Definition
import Pair.Definition
import Pair.Examples

idD :: Text
idD = T.unlines
  [ "id = rec Nat to Nat where"
  , "       { Zero x = Zero @ ()"
  , "       ; Suc x = Suc @ x }"
  ]

idExpr :: Expr
idExpr = Iter { ductive = natDuc
              , parameters = []
              , motive = GlobalTypeVar "Nat" []
              , matches = [ (["x"], zeroExpr)
                          , ( ["x"]
                            , sucExpr :@: LocalExprVar 0 False "x")]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [natD] idD
      [ ExprDef { name = "id"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = idExpr
                , ty = Nothing}]
  it "Type checks id to (Nat) -> Nat" $
    shouldCheckWithDefs [natD] idExpr
      ([natExpr], GlobalTypeVar "Nat" [])
  it "Type checks id on zero to Nat" $
    shouldCheckWithDefs [natD] (idExpr :@: zeroExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates id on zero to zero" $
    shouldEvalWithDefs [natD] (idExpr :@: zeroExpr)
      zeroExpr
  it "Type checks id on one to Nat" $
    shouldCheckWithDefs [zeroDR] (idExpr :@: oneExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates id on one to inlined one" $
    shouldEvalWithDefs [zeroDR] (idExpr :@: oneExpr)
      oneExprI
  it "Evaluates id on inlined one to inlined one" $
    shouldEvalWithDefs [natD] (idExpr :@: oneExprI)
      oneExprI
  it "Evaluates id on two to inlined two" $
    shouldEvalWithDefs [oneDR] (idExpr :@: twoExpr)
      twoExprI
  it "Evaluates id on inlined two to inlined two" $
    shouldEvalWithDefs [natD] (idExpr :@: twoExprI)
      twoExprI
  it "id works for any number" $ hedgehog $ do
      n <- forAll $ Gen.integral (Range.linear 0 1000)
      shouldEvalWithDefsP [packedD, pairD, zeroDR] (idExpr :@: genNatExpr n)
        (genNatExpr n)

plusD :: Text
plusD = T.unlines
  [ "plus = rec<Pair<Nat,Nat>> Packed to Nat where"
  , "         Pack x = (rec Nat to Nat where"
  , "                     { Zero u = Second<Nat,Nat> @ x"
  , "                     ; Suc n = Suc @ n}) @ (First<Nat,Nat> @ x)" ]

plusExpr :: Expr
plusExpr =
  Iter { ductive = packedDuc
       , parameters = [GlobalTypeVar "Pair" [ GlobalTypeVar "Nat" []
                                            , GlobalTypeVar "Nat" []]]
       , motive = GlobalTypeVar "Nat" []
       , matches =
          [( ["x"]
           , Iter { ductive = natDuc
                  , parameters = []
                  , motive = GlobalTypeVar "Nat" []
                  , matches = [ ( ["u"]
                                  , sndExpr (GlobalTypeVar "Nat" [])
                                            (GlobalTypeVar "Nat" [])
                                    :@: LocalExprVar 1 False "x")
                              , ( ["n"]
                                , sucExpr :@: LocalExprVar 0 False "n")]}
             :@: (fstExpr (GlobalTypeVar "Nat" []) (GlobalTypeVar "Nat" [])
                  :@: LocalExprVar 0 False "x"))]}

plusTests :: Spec
plusTests = do
  it "Parses plus" $
    shouldParseWithDefs [packedD, pairD, natD] plusD
      [ ExprDef { name = "plus"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = plusExpr
                , ty = Nothing}]
  it "Type checks plus to (Nat x Nat) -> Nat" $
    shouldCheckWithDefs [packedD, pairD, natD] plusExpr
      ([packedExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Nat" []
                                         , GlobalTypeVar "Nat" []])]
      , GlobalTypeVar "Nat" [])
  let natPair x y =
        packExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Nat" []
                                       , GlobalTypeVar "Nat" []])
        :@: mkPairExpr (GlobalTypeVar "Nat" []) (GlobalTypeVar "Nat" []) x y
  it "Type checks plus on zero x zero to Nat" $
    shouldCheckWithDefs [packedD, pairD, natD] (plusExpr
                                                 :@: natPair zeroExpr zeroExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates plus on zero x zero to zero" $
    shouldEvalWithDefs [packedD, pairD, natD] (plusExpr
                                                :@: natPair zeroExpr zeroExpr)
      zeroExpr
  it "Type checks plus on zero x one to Nat" $
    shouldCheckWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                 :@: natPair zeroExpr oneExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates plus on zero x one to one" $
    shouldEvalWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                :@: natPair zeroExpr oneExpr)
      oneExprI
  it "Type checks plus on one x zero to Nat" $
    shouldCheckWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                 :@: natPair oneExpr zeroExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates plus on one x zero to one" $
    shouldEvalWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                :@: natPair oneExpr zeroExpr)
      oneExprI
  it "Type checks plus on one x one to Nat" $
    shouldCheckWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                 :@: natPair oneExpr oneExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates plus on one x one to two" $
    shouldEvalWithDefs [packedD, pairD, zeroDR] (plusExpr
                                                :@: natPair oneExpr oneExpr)
      twoExprI
  it "plus works for any number" $ hedgehog $ do
      n <- forAll $ Gen.integral (Range.linear 0 1000)
      m <- forAll $ Gen.integral (Range.linear 0 1000)
      shouldEvalWithDefsP [packedD, pairD, zeroDR] (plusExpr
                                                    :@: natPair (genNatExpr n)
                                                                (genNatExpr m))
        (genNatExpr (n+m))
