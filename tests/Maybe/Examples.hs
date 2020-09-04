{-# language OverloadedStrings#-}
module Maybe.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Maybe.Definition

maybeEx1D, maybeEx1DR, maybeEx2D, maybeEx2DR :: Text
maybeEx3D, maybeEx3DR, maybeEx4D, maybeEx4DR :: Text
maybeEx1D = "maybe1 = Nothing<Unit> @ ()"
maybeEx1DR = T.unlines [maybeD, maybeEx1D]
maybeEx2D = "maybe2 = Just<Unit> @ ()"
maybeEx2DR = T.unlines [maybeEx1DR, maybeEx2D]
maybeEx3D = "maybe3 = Nothing<Nat> @ ()"
maybeEx3DR = T.unlines [natD, maybeD, maybeEx3D]
maybeEx4D = "maybe4 = Just<Nat> @ one"
maybeEx4DR = T.unlines [oneDR, maybeD, maybeEx4D]

maybeEx1Expr, maybeEx1ExprI, maybeEx2Expr, maybeEx2ExprI :: Expr
maybeEx3Expr, maybeEx3ExprI, maybeEx4Expr, maybeEx4ExprI :: Expr
maybeEx1Expr = WithParameters [UnitType] (Constructor maybeDucA 0)
              :@: UnitExpr
maybeEx1ExprI = Constructor (maybeDuc UnitType) 0 :@: UnitExpr
maybeEx2Expr = WithParameters [UnitType] (Constructor maybeDucA 1)
              :@: UnitExpr
maybeEx2ExprI = Constructor (maybeDuc UnitType) 1 :@: UnitExpr
maybeEx3Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor maybeDucA 0)
              :@: UnitExpr
maybeEx3ExprI = Constructor (maybeDuc natExpr) 0 :@: UnitExpr
maybeEx4Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor maybeDucA 1)
              :@: GlobalExprVar "one" [] []
maybeEx4ExprI = Constructor (maybeDuc natExpr) 1 :@: oneExprI

maybeExTests :: Spec
maybeExTests = do
  it "Parses Nothing of units" $
    shouldParseWithDefs [maybeD] maybeEx1D
      [ ExprDef { name = "maybe1"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = maybeEx1Expr
                , ty = Nothing}]
  it "Type checks Nothing of units to Maybe<Unit>" $
    shouldCheckWithDefs [maybeD] maybeEx1Expr
      ([], maybeExpr UnitType)
  it "Parses a maybe with one unit" $
    shouldParseWithDefs [maybeEx1DR] maybeEx2D
      [ ExprDef { name = "maybe2"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = maybeEx2Expr
                , ty = Nothing}]
  it "Type checks a maybe with one unit to Maybe<Unit>" $
    shouldCheckWithDefs [maybeEx1DR] maybeEx2Expr
      ([], maybeExpr UnitType)
  it "Parses a empty maybe of nats" $
    shouldParseWithDefs [natD, maybeD] maybeEx3D
      [ ExprDef { name = "maybe3"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = maybeEx3Expr
                , ty = Nothing}]
  it "Type checks a empty maybe of nats to Maybe<Nat>" $
    shouldCheckWithDefs [natD, maybeD] maybeEx3Expr
      ([], maybeExpr (GlobalTypeVar "Nat" []))
  it "Parses a maybe with one number one" $
    shouldParseWithDefs [maybeEx3DR, zeroD, oneD] maybeEx4D
      [ ExprDef { name = "maybe4"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = maybeEx4Expr
                , ty = Nothing}]
  it "Type checks a maybe with one number one to Maybe<Nat>" $
    shouldCheckWithDefs [maybeEx3DR, zeroD, oneD] maybeEx4Expr
      ([], maybeExpr (GlobalTypeVar "Nat" []))

