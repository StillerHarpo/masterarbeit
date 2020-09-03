{-# language OverloadedStrings#-}
module Pair.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree
import TypeChecker

import Lib
import Nat.Definition
import Nat.Examples
import Packed.Definition
import Maybe.Definition
import List.Definition
import List.Examples
import Pair.Definition
import Pair.Examples

destructorTests :: Spec
destructorTests = do
  let listPair x y tyX tyY = mkPairExpr (GlobalTypeVar "List" [tyX])
                                        (GlobalTypeVar "List" [tyY]) x y
  it "Type checks Evaluation of First of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
                      (evalExpr (WithParameters [ UnitType , UnitType]
                                                (Destructor pairDucAB 0)
                                 :@: listPair listEx1Expr
                                              listEx1Expr
                                              UnitType
                                              UnitType)
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([],[]) to [Unit]" $
    shouldRunWithDefs [pairD, listD]
                      (evalExpr (WithParameters [ UnitType , UnitType]
                                                (Destructor pairDucAB 1)
                                 :@: listPair listEx1Expr
                                              listEx1Expr
                                              UnitType
                                              UnitType)
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[]) to [Unit]" $
    shouldRunWithDefs [listEx1DR]
                      (evalExpr (WithParameters [ UnitType , UnitType]
                                                (Destructor pairDucAB 0)
                                 :@: listPair listEx2Expr
                                              listEx1Expr
                                              UnitType
                                              UnitType)
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([1,2],[()]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
                      (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                                , UnitType ]
                                                (Destructor pairDucAB 0)
                                 :@: listPair listEx5Expr
                                              listEx2Expr
                                              (GlobalTypeVar "Nat" [])
                                              UnitType)
                       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[()]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
                      (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                                , UnitType ]
                                                (Destructor pairDucAB 1)
                                 :@: listPair listEx5Expr
                                              listEx2Expr
                                              (GlobalTypeVar "Nat" [])
                                              UnitType)
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of First of ([()],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
                      (evalExpr (WithParameters [ UnitType
                                                , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                                (Destructor pairDucAB 0)
                                 :@: listPair listEx2Expr
                                              listEx5Expr
                                              UnitType
                                              (GlobalTypeVar "Nat" []))
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of Second of ([()],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, listEx1D, twoD]
                      (evalExpr (WithParameters [ UnitType
                                                , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                                (Destructor pairDucAB 1)
                                 :@: listPair listEx2Expr
                                              listEx5Expr
                                              UnitType
                                              (GlobalTypeVar "Nat" []))
                       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of First of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
                      (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                                , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                                (Destructor pairDucAB 0)
                                 :@: listPair listEx5Expr
                                              listEx5Expr
                                              (GlobalTypeVar "Nat" [])
                                              (GlobalTypeVar "Nat" []))
                       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks Evaluation of Second of ([1,2],[1,2]) to [Nat]" $
    shouldRunWithDefs [listEx4DR, twoD]
                      (evalExpr (WithParameters [ GlobalTypeVar "List" [GlobalTypeVar "Nat" []]
                                                , GlobalTypeVar "List" [GlobalTypeVar "Nat" []]]
                                                (Destructor pairDucAB 1)
                                 :@: listPair listEx5Expr
                                              listEx5Expr
                                              (GlobalTypeVar "Nat" [])
                                              (GlobalTypeVar "Nat" []))
                       >>= inferTerm)
      ([], listExpr (GlobalTypeVar "Nat" []))
