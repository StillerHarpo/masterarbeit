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
                        (WithParameters [ UnitType , UnitType]
                                        (Destructor pairDucAB 0)
                         :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      ([], UnitType)
  it "Evals First of (Unit,Unit) to Unit" $
    shouldEvalWithDefs [pairD, listD]
                      (WithParameters [ UnitType , UnitType]
                                       (Destructor pairDucAB 0)
                       :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      UnitExpr
  it "Type checks Second of (Unit,Unit) to Unit" $
    shouldCheckWithDefs [pairD, listD]
                        (WithParameters [ UnitType , UnitType]
                                        (Destructor pairDucAB 1)
                         :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      ([], UnitType)
  it "Evals Second of (Unit,Unit) to Unit" $
    shouldEvalWithDefs [pairD, listD]
                      (WithParameters [ UnitType , UnitType]
                                      (Destructor pairDucAB 1)
                       :@: mkPairExpr UnitType UnitType UnitExpr UnitExpr)
      UnitExpr
  let boolPair = mkPairExpr (GlobalTypeVar "Bool" [])
                            (GlobalTypeVar "Bool" [])
  it "Type checks First of (True,False) to Bool" $
    shouldCheckWithDefs [pairD, boolD]
                        (WithParameters [ GlobalTypeVar "Bool" []
                                        , GlobalTypeVar "Bool" []]
                                        (Destructor pairDucAB 0)
                         :@: boolPair trueExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evals First of (True,False) to True" $
    shouldEvalWithDefs [pairD, boolD]
                       (WithParameters [ GlobalTypeVar "Bool" []
                                       , GlobalTypeVar "Bool" []]
                                       (Destructor pairDucAB 0)
                        :@: boolPair trueExpr falseExpr)
      trueExpr
  it "Type checks Second of (True,False) to Bool" $
    shouldCheckWithDefs [pairD, boolD]
                        (WithParameters [ GlobalTypeVar "Bool" []
                                        , GlobalTypeVar "Bool" []]
                                        (Destructor pairDucAB 1)
                         :@: boolPair trueExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evals Second of (True,False) to False" $
    shouldEvalWithDefs [pairD, boolD]
                       (WithParameters [ GlobalTypeVar "Bool" []
                                                 , GlobalTypeVar "Bool" []]
                                                 (Destructor pairDucAB 1)
                        :@: boolPair trueExpr falseExpr)
      falseExpr
  let listPair x y tyX tyY = mkPairExpr (GlobalTypeVar "List" [tyX])
                                        (GlobalTypeVar "List" [tyY]) x y
  it "Type checks First of ([],[]) to [Unit]" $
    shouldCheckWithDefs [pairD, listD]
                        (WithParameters [ GlobalTypeVar "List" [UnitType]
                                        , GlobalTypeVar "List" [UnitType] ]
                                        (Destructor pairDucAB 0)
                         :@: listPair listEx1Expr
                                      listEx1Expr
                                      UnitType
                                      UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Type checks Evaluation of First of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [UnitType]
                                  , GlobalTypeVar "List" [UnitType] ]
                                  (Destructor pairDucAB 0)
                  :@: listPair listEx1Expr
                               listEx1Expr
                               UnitType
                               UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [UnitType]
                                  , GlobalTypeVar "List" [UnitType] ]
                                  (Destructor pairDucAB 1)
                   :@: listPair listEx1Expr
                                listEx1Expr
                                UnitType
                                UnitType))
         >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[]) to [Unit]" $
    shouldRunWithDefs [listEx1DR]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [UnitType]
                                  , GlobalTypeVar "List" [UnitType] ]
                                  (Destructor pairDucAB 0)
                   :@: listPair listEx2Expr
                                listEx1Expr
                                UnitType
                                UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([1,2],[()]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                  , GlobalTypeVar "List" [UnitType] ]
                                  (Destructor pairDucAB 0)
                   :@: listPair listEx5Expr
                                listEx2Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[()]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                  , GlobalTypeVar "List" [UnitType] ]
                                  (Destructor pairDucAB 1)
                   :@: listPair listEx5Expr
                                listEx2Expr
                                (GlobalTypeVar "Nat" [])
                                UnitType))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
        (evalExpr (WithParameters [ GlobalTypeVar "List" [UnitType]
                                  , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                  (Destructor pairDucAB 0)
                   :@: listPair listEx2Expr
                                listEx5Expr
                                UnitType
                                (GlobalTypeVar "Nat" [])))
        >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([()],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
      (evalInTI
         (evalExpr (WithParameters [ GlobalTypeVar "List" [UnitType]
                                   , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                   (Destructor pairDucAB 1)
                    :@: listPair listEx2Expr
                                 listEx5Expr
                                 UnitType
                                 (GlobalTypeVar "Nat" [])))
          >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of First of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
      (evalInTI
         (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                   , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                   (Destructor pairDucAB 0)
                    :@: listPair listEx5Expr
                                 listEx5Expr
                                 (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])))
       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
      (evalInTI
         (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                   , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                   (Destructor pairDucAB 1)
                    :@: listPair listEx5Expr
                                 listEx5Expr
                                 (GlobalTypeVar "Nat" [])
                                 (GlobalTypeVar "Nat" [])))
       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
