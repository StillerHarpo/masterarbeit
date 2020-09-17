{-# language OverloadedStrings#-}
module Pair.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree
import TypeChecker
import Eval

import Lib
import Nat.Definition
import Nat.Examples
import Packed.Definition
import Maybe.Definition
import Bool.Definition
import Bool.Examples
import List.Definition
import List.Examples
import Pair.Definition
import Pair.Examples

destructorTests :: Spec
destructorTests = do
  it "Type checks First of (Unit,Unit) to Unit" $
    shouldCheckWithDefs [pairD, listD]
                        (fstExpr UnitType UnitType
                         :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      ([], UnitType)
  it "Evals First of (Unit,Unit) to Unit" $
    shouldEvalWithDefs [pairD, listD]
                      (fstExpr UnitType UnitType
                       :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      UnitExpr
  it "Type checks Second of (Unit,Unit) to Unit" $
    shouldCheckWithDefs [pairD, listD]
                        (sndExpr UnitType UnitType
                         :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      ([], UnitType)
  it "Evals Second of (Unit,Unit) to Unit" $
    shouldEvalWithDefs [pairD, listD]
                       (sndExpr UnitType UnitType
                        :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      UnitExpr
  let boolPair = mkPairExpr (GlobalTypeVar "Bool" [])
                            (GlobalTypeVar "Bool" [])
  it "Type checks First of (True,False) to Bool" $
    shouldCheckWithDefs [pairD, boolD]
                        (fstExpr (GlobalTypeVar "Bool" [])
                                 (GlobalTypeVar "Bool" [])
                         :@: boolPair trueExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evals First of (True,False) to True" $
    shouldEvalWithDefs [pairD, boolD]
                       (fstExpr (GlobalTypeVar "Bool" [])
                                (GlobalTypeVar "Bool" [])
                        :@: boolPair trueExpr falseExpr)
      trueExpr
  it "Type checks Second of (True,False) to Bool" $
    shouldCheckWithDefs [pairD, boolD]
                        (sndExpr (GlobalTypeVar "Bool" [])
                                 (GlobalTypeVar "Bool" [])
                         :@: boolPair trueExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evals Second of (True,False) to False" $
    shouldEvalWithDefs [pairD, boolD]
                       (sndExpr (GlobalTypeVar "Bool" [])
                                (GlobalTypeVar "Bool" [])
                        :@: boolPair trueExpr falseExpr)
      falseExpr
  let natPair = mkPairExpr (GlobalTypeVar "Nat" [])
                           (GlobalTypeVar "Nat" [])
  it "Evals First of (zero,zero) to zero" $
    shouldEvalWithDefs [pairD, boolD, natD]
                       (fstExpr (GlobalTypeVar "Nat" [])
                                (GlobalTypeVar "Nat" [])
                        :@: natPair zeroExpr zeroExpr)
      zeroExpr
  it "Evals First of (one,zero) to one" $
    shouldEvalWithDefs [pairD, boolD, oneDR]
                       (fstExpr (GlobalTypeVar "Nat" [])
                                (GlobalTypeVar "Nat" [])
                        :@: natPair oneExpr zeroExpr)
      oneExprI
  it "Type checks First of (one,two) to Nat" $
    shouldCheckWithDefs [pairD, boolD, twoDR]
                        (fstExpr (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])
                         :@: natPair oneExpr twoExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evals First of (one,two) to two" $
    shouldEvalWithDefs [pairD, boolD, twoDR]
                       (fstExpr (GlobalTypeVar "Nat" [])
                                (GlobalTypeVar "Nat" [])
                        :@: natPair oneExpr twoExpr)
      oneExprI
  it "Type checks Second of (True,False) to Nat" $
    shouldCheckWithDefs [pairD, boolD, twoDR]
                        (sndExpr (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])
                         :@: natPair oneExpr twoExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evals Second of (1,2) to 2" $
    shouldEvalWithDefs [pairD, boolD, twoDR]
                       (sndExpr (GlobalTypeVar "Nat" [])
                                (GlobalTypeVar "Nat" [])
                        :@: natPair oneExpr twoExpr)
      twoExprI
  let listPair x y tyX tyY = mkPairExpr (GlobalTypeVar "List" [tyX])
                                        (GlobalTypeVar "List" [tyY]) x y
  it "Type checks First of ([],[]) to [Unit]" $
    shouldCheckWithDefs [pairD, listD]
                        (fstExpr (GlobalTypeVar "List" [UnitType])
                                 (GlobalTypeVar "List" [UnitType])
                         :@: listPair listEx1Expr
                                      listEx1Expr
                                      UnitType
                                      UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Type checks Evaluation of First of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [UnitType])
                           (GlobalTypeVar "List" [UnitType])
                  :@: listPair listEx1Expr
                               listEx1Expr
                               UnitType
                               UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
      (evalInTI
        (evalExpr (sndExpr (GlobalTypeVar "List" [UnitType])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx1Expr
                                listEx1Expr
                                UnitType
                                UnitType))
         >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[]) to [Unit]" $
    shouldRunWithDefs [listEx1DR]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [UnitType])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx2Expr
                                listEx1Expr
                                UnitType
                                UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([],[]) to [Nat]" $
    shouldRunWithDefs [listEx3DR]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx3Expr
                                listEx3Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of First of ([1],[]) to [Nat]" $
    shouldRunWithDefs [listEx4DR]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx4Expr
                                listEx3Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of First of ([1,2],[()]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx5Expr
                                listEx2Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[()]) to [Unit]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (sndExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                           (GlobalTypeVar "List" [UnitType])
                   :@: listPair listEx5Expr
                                listEx2Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[1,2]) to [Unit]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (fstExpr (GlobalTypeVar "List" [UnitType])
                           (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                   :@: listPair listEx2Expr
                                listEx5Expr
                                UnitType
                                (GlobalTypeVar "Nat" [])))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([()],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
         (evalExpr (sndExpr (GlobalTypeVar "List" [UnitType])
                            (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                    :@: listPair listEx2Expr
                                 listEx5Expr
                                 UnitType
                                 (GlobalTypeVar "Nat" [])))
          >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of First of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
      (evalInTI
         (evalExpr (fstExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                            (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                    :@: listPair listEx5Expr
                                 listEx5Expr
                                 (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])))
       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
      (evalInTI
         (evalExpr (sndExpr (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                            (GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
                    :@: listPair listEx5Expr
                                 listEx5Expr
                                 (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])))
       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
