{-# language OverloadedStrings#-}
module Nat.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

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
  , "       { Zero x = Zero @ x"
  , "       ; Suc x = Suc @ x }"
  ]

idExpr :: Expr
idExpr = Rec { fromRec = natDuc
             , toRec = GlobalTypeVar "Nat" []
             , matches = [ Constructor natDuc 0 :@: LocalExprVar 0 "x"
                         , Constructor natDuc 1 :@: LocalExprVar 0 "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [natD] idD
      [ ExprDef { name = "id"
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

plusD :: Text
plusD = T.unlines
  [ "plus = rec<Pair<Nat,Nat>> Packed to Nat where"
  , "         Pack x = (rec Nat to Nat where"
  , "                     { Zero u = Second<Nat,Nat> @ x"
  , "                     ; Suc n = Suc @ n}) @ (First<Nat,Nat> @ x)" ]

plusExpr :: Expr
plusExpr = WithParameters [GlobalTypeVar "Pair" [ GlobalTypeVar "Nat" []
                                                , GlobalTypeVar "Nat" []]] $
  Rec { fromRec = packedDucA
      , toRec = GlobalTypeVar "Nat" []
      , matches =
          [Rec { fromRec = natDuc
               , toRec = GlobalTypeVar "Nat" []
               , matches = [ WithParameters [ GlobalTypeVar "Nat" []
                                            , GlobalTypeVar "Nat" [] ]
                                            (Destructor pairDucAB 1)
                             :@: LocalExprVar 1 "x"
                           , Constructor natDuc 1 :@: LocalExprVar 0 "n"]}
           :@: (WithParameters [ GlobalTypeVar "Nat" []
                               , GlobalTypeVar "Nat" []]
                               (Destructor pairDucAB 0)
               :@: LocalExprVar 0 "x")]}

plusTests :: Spec
plusTests = do
  it "Parses plus" $
    shouldParseWithDefs [packedD, pairD, natD] plusD
      [ ExprDef { name = "plus"
                , expr = plusExpr
                , ty = Nothing}]
  it "Type checks plus to (Nat x Nat) -> Nat" $
    shouldCheckWithDefs [packedD, pairD, natD] plusExpr
      ([packedExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Nat" []
                                         , GlobalTypeVar "Nat" []])]
      , GlobalTypeVar "Nat" [])
  let natPair x y =
        Constructor (packedDuc (GlobalTypeVar "Pair"
                                               [ GlobalTypeVar "Nat" []
                                               , GlobalTypeVar "Nat" []]))
                    0
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
