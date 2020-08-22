{-# language OverloadedStrings#-}
module Pair.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Pair.Definition

mkPairD :: Text -> Text -> Text -> Text -> Text
mkPairD tyX tyY x y = T.unlines
  -- TODO make block parsing work in nested cases
  [ "((corec<" <> tyX <> "," <> tyY <> "> Unit to Pair where"
  , "   { First = " <> x
  , "   ; Second = " <> y <> "}) @ ())"]

mkPairExpr :: TypeExpr -> TypeExpr -> Expr -> Expr -> Expr
mkPairExpr tyX tyY x y = WithParameters [tyX, tyY]
                                        (Corec { fromCorec = UnitType
                                               , toCorec = pairDucAB
                                               , matches = [x, y]})
                         :@: UnitExpr

pairEx1D, pairEx2D, pairEx3D, pairEx4D :: Text
pairEx1D = "pair1 = " <> mkPairD "Unit" "Unit" "()" "()"
pairEx2D = "pair2 = " <> mkPairD "Unit" "Nat" "()" "one"
pairEx3D = "pair3 = " <> mkPairD "Nat" "Unit" "one" "()"
pairEx4D = "pair4 = " <> mkPairD "Nat" "Nat" "one" "two"

pairEx1Expr, pairEx2Expr, pairEx3Expr, pairEx4Expr :: Expr
pairEx1Expr = mkPairExpr UnitType UnitType UnitExpr UnitExpr
pairEx2Expr = mkPairExpr UnitType (GlobalTypeVar "Nat" [])
                         UnitExpr (GlobalExprVar "one")
pairEx3Expr = mkPairExpr (GlobalTypeVar "Nat" []) UnitType
                         (GlobalExprVar "one") UnitExpr
pairEx4Expr = mkPairExpr (GlobalTypeVar "Nat" [])
                         (GlobalTypeVar "Nat" [])
                         (GlobalExprVar "one")
                         (GlobalExprVar "two")

pairExTest :: Spec
pairExTest = do
  it "Parses pair of units" $
    shouldParseWithDefs [pairD] pairEx1D
      [ ExprDef { name = "pair1"
                , expr = pairEx1Expr
                , ty = Nothing}]
  it "Type checks a pair of units to Pair<Unit,Unit>" $
    shouldCheckWithDefs [pairD] pairEx1Expr
      ([], pairExpr UnitType UnitType)
  it "Parses pair of unit and one" $
    shouldParseWithDefs [oneDR, pairD] pairEx2D
      [ ExprDef { name = "pair2"
                , expr = pairEx2Expr
                , ty = Nothing}]
  it "Type checks a pair of unit and one to Pair<Unit,Nat>" $
    shouldCheckWithDefs [oneDR, pairD] pairEx2Expr
      ([], pairExpr UnitType (GlobalTypeVar "Nat" []))
  it "Parses pair of one and unit" $
    shouldParseWithDefs [oneDR, pairD] pairEx3D
      [ ExprDef { name = "pair3"
                , expr = pairEx3Expr
                , ty = Nothing}]
  it "Type checks a pair of one and unit to Pair<Nat,Unit>" $
    shouldCheckWithDefs [oneDR, pairD] pairEx3Expr
      ([], pairExpr (GlobalTypeVar "Nat" []) UnitType)
  it "Parses pair of one and two" $
    shouldParseWithDefs [twoDR, pairD] pairEx4D
      [ ExprDef { name = "pair4"
                , expr = pairEx4Expr
                , ty = Nothing}]
  it "Type checks a pair of one and two to Pair<Nat,Nat>" $
    shouldCheckWithDefs [twoDR, pairD] pairEx4Expr
      ([], pairExpr (GlobalTypeVar "Nat" []) (GlobalTypeVar "Nat" []))

