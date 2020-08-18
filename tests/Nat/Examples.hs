{-# language OverloadedStrings#-}
module Nat.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition

zeroD, zeroDR, oneD, oneDR, twoD, twoDR :: Text
zeroD = "zero = Zero @ ()"
zeroDR = T.unlines [natD, zeroD]
oneD = "one = Suc @ zero"
oneDR = T.unlines [zeroDR, oneD]
twoD = "two = Suc @ one"
twoDR = T.unlines [oneDR, twoD]

zeroExpr, oneExpr, oneExprI, twoExpr, twoExprI :: Expr
zeroExpr = Constructor natDuc 0 :@: UnitExpr
oneExpr = Constructor natDuc 1 :@: GlobalExprVar "zero"
oneExprI = Constructor natDuc 1 :@: zeroExpr
twoExpr = Constructor natDuc 1 :@: GlobalExprVar "one"
twoExprI = Constructor natDuc 1 :@: oneExprI

natExTests :: Spec
natExTests = do
  it "Parses zero" $
    shouldParseWithDefs [natD] zeroD
      [ ExprDef { name = "zero"
                , expr = zeroExpr
                , ty = Nothing}]
  it "Type checks zero to nat" $
    shouldCheckWithDefs [natD] zeroExpr
      ([], natExpr)
  it "Parses one" $
    shouldParseWithDefs [zeroDR] oneD
      [ ExprDef { name = "one"
                , expr = oneExpr
                , ty = Nothing}]
  it "Type checks one to nat" $
    shouldCheckWithDefs [zeroDR] oneExpr
      ([], natExpr)
  it "Type checks inlined one to nat" $
    shouldCheckWithDefs [natD] oneExprI
      ([], natExpr)
  it "Parses two" $
    shouldParseWithDefs [oneDR] twoD
      [ ExprDef { name = "two"
                , expr = twoExpr
                , ty = Nothing}]
  it "Type checks two to nat" $
    shouldCheckWithDefs [oneDR] twoExpr
      ([], natExpr)
  it "Type checks inlined two to nat" $
    shouldCheckWithDefs [natD] twoExprI
      ([], natExpr)
