{-# language OverloadedStrings#-}
module Packed.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Packed.Definition
import Nat.Definition
import Nat.Examples


packedEx1D, packedEx1DR, packedEx2D, packedEx2DR :: Text
packedEx1D = "packed1 = Pack<Unit> @ ()"
packedEx1DR = T.unlines [packedD, packedEx1D]
packedEx2D = "packed2 = Pack<Nat> @ one"
packedEx2DR = T.unlines [oneDR, packedD, packedEx2D]

packedEx1Expr, packedEx1ExprI, packedEx2Expr, packedEx2ExprI :: Expr
packedEx1Expr = WithParameters [UnitType] (Constructor packedDucA 0)
                :@: UnitExpr
packedEx1ExprI = Constructor (packedDuc UnitType) 0 :@: UnitExpr
packedEx2Expr = WithParameters [GlobalTypeVar "Nat" []]
                             (Constructor packedDucA 0)
                :@: GlobalExprVar "one" [] []
packedEx2ExprI = Constructor (packedDuc natExpr) 0 :@: oneExprI

packedExTests :: Spec
packedExTests = do
  it "Parses a packed unit" $
    shouldParseWithDefs [packedD] packedEx1D
      [ ExprDef { name = "packed1"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = packedEx1Expr
                , ty = Nothing}]
  it "Type checks a packed unit to Packed<Unit>" $
    shouldCheckWithDefs [packedD] packedEx1Expr
      ([], packedExpr UnitType)
  it "Parses a packed one" $
    shouldParseWithDefs [oneDR, packedD] packedEx2D
      [ ExprDef { name = "packed2"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = packedEx2Expr
                , ty = Nothing}]
  it "Type checks a packed one to Packed<Nat>" $
    shouldCheckWithDefs [oneDR, packedD] packedEx2Expr
      ([], packedExpr (GlobalTypeVar "Nat" []))


