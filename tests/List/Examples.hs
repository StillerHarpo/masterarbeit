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

listEx1D, listEx2D, listEx3D, listEx4D :: Text
listEx1D = "list1 = Nil<Unit> @ ()"
listEx2D = "list2 = Cons<Unit> @ "
            <> mkPairD "Unit" "List<Unit>" "()" "list1"
listEx3D = "list3 = Nil<Nat> @ ()"
listEx4D = "list4 = Cons<Nat> @ "
            <> mkPairD "Nat" "List<Nat>" "one" "list3"

listEx1Expr, listEx2Expr, listEx3Expr, listEx4Expr :: Expr
listEx1Expr = WithParameters [UnitType] (Constructor listDuc 0)
              :@: UnitExpr
listEx2Expr = WithParameters [UnitType] (Constructor listDuc 1)
              :@: mkPairExpr UnitType
                             (GlobalTypeVar "List" [UnitType])
                             UnitExpr
                             (GlobalExprVar "list1")
listEx3Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor listDuc 0)
              :@: UnitExpr
listEx4Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor listDuc 1)
              :@: mkPairExpr (GlobalTypeVar "Nat" [])
                             (GlobalTypeVar "List"
                                            [GlobalTypeVar "Nat" []])
                             (GlobalExprVar "one")
                             (GlobalExprVar "list3")

listExTest :: Spec
listExTest = do
  it "Parses a empty list of units" $
    shouldParseWithDefs [pairD, listD] listEx1D
      [ ExprDef { name = "list1"
                , expr = listEx1Expr
                , ty = Nothing}]
  it "Type checks a empty list of units to List<Unit>" $
    shouldCheckWithDefs [pairD, listD] listEx1Expr
      ([], GlobalTypeVar "List" [UnitType])
  it "Parses a list with one unit" $
    shouldParseWithDefs [pairD, listD, listEx1D] listEx2D
      [ ExprDef { name = "list2"
                , expr = listEx2Expr
                , ty = Nothing}]
  it "Type checks a list with one unit to List<Unit>" $
    shouldCheckWithDefs [pairD, listD, listEx1D] listEx2Expr
      ([], GlobalTypeVar "List" [UnitType])
  it "Parses a empty list of nats" $
    shouldParseWithDefs [natD, pairD, listD] listEx3D
      [ ExprDef { name = "list3"
                , expr = listEx3Expr
                , ty = Nothing}]
  it "Type checks a empty list of nats to List<Nat>" $
    shouldCheckWithDefs [natD, pairD, listD] listEx3Expr
      ([], GlobalTypeVar "List" [GlobalTypeVar "Nat" []])
  it "Parses a list with one number one" $
    shouldParseWithDefs [oneDR, pairD, listD, listEx3D] listEx4D
      [ ExprDef { name = "list4"
                , expr = listEx4Expr
                , ty = Nothing}]
  it "Type checks a list with one number one to List<Nat>" $
    shouldCheckWithDefs [oneDR, pairD, listD, listEx3D] listEx4Expr
      ([], GlobalTypeVar "List" [GlobalTypeVar "Nat" []])

