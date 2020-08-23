{-# language OverloadedStrings#-}
module List.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Pair.Definition
import Pair.Examples
import List.Definition

listEx1D, listEx1DR, listEx2D, listEx2DR :: Text
listEx3D, listEx3DR, listEx4D, listEx4DR :: Text
listEx1D = "list1 = Nil<Unit> @ ()"
listEx1DR = T.unlines [listDR, listEx1D]
listEx2D = "list2 = Cons<Unit> @ "
            <> mkPairD "Unit" "List<Unit>" "()" "list1"
listEx2DR = T.unlines [listEx1DR, listEx2D]
listEx3D = "list3 = Nil<Nat> @ ()"
listEx3DR = T.unlines [natD, listDR, listEx3D]
listEx4D = "list4 = Cons<Nat> @ "
            <> mkPairD "Nat" "List<Nat>" "one" "list3"
listEx4DR = T.unlines [oneDR, listDR, listEx4D]

listEx1Expr, listEx2Expr, listEx3Expr, listEx4Expr :: Expr
listEx1Expr = WithParameters [UnitType] (Constructor listDucA 0)
              :@: UnitExpr
listEx2Expr = WithParameters [UnitType] (Constructor listDucA 1)
              :@: mkPairExpr UnitType
                             (GlobalTypeVar "List" [UnitType])
                             UnitExpr
                             (GlobalExprVar "list1")
listEx3Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor listDucA 0)
              :@: UnitExpr
listEx4Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor listDucA 1)
              :@: mkPairExpr (GlobalTypeVar "Nat" [])
                             (GlobalTypeVar "List"
                                            [GlobalTypeVar "Nat" []])
                             (GlobalExprVar "one")
                             (GlobalExprVar "list3")

listExTest :: Spec
listExTest = do
  it "Parses a empty list of units" $
    shouldParseWithDefs [listDR] listEx1D
      [ ExprDef { name = "list1"
                , expr = listEx1Expr
                , ty = Nothing}]
  it "Type checks a empty list of units to List<Unit>" $
    shouldCheckWithDefs [listDR] listEx1Expr
      ([], listExpr UnitType)
  it "Parses a list with one unit" $
    shouldParseWithDefs [listEx1DR] listEx2D
      [ ExprDef { name = "list2"
                , expr = listEx2Expr
                , ty = Nothing}]
  it "Type checks a list with one unit to List<Unit>" $
    shouldCheckWithDefs [listEx1DR] listEx2Expr
      ([], listExpr UnitType)
  it "Parses a empty list of nats" $
    shouldParseWithDefs [natD, listDR] listEx3D
      [ ExprDef { name = "list3"
                , expr = listEx3Expr
                , ty = Nothing}]
  it "Type checks a empty list of nats to List<Nat>" $
    shouldCheckWithDefs [natD, listDR] listEx3Expr
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Parses a list with one number one" $
    shouldParseWithDefs [listEx3DR, zeroD, oneD] listEx4D
      [ ExprDef { name = "list4"
                , expr = listEx4Expr
                , ty = Nothing}]
  it "Type checks a list with one number one to List<Nat>" $
    shouldCheckWithDefs [listEx3DR, zeroD, oneD] listEx4Expr
      ([], listExpr (GlobalTypeVar "Nat" []))

