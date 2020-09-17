{-# language OverloadedStrings#-}
module Stream.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Maybe.Definition
import Pair.Definition
import Stream.Definition
import Stream.Examples

first :: TypeExpr -> Expr -> Expr
first ty str = Structor { ductive = streamDuc
                        , parameters = [ty]
                        , num = 0 } :@: str

second :: TypeExpr -> Expr-> Expr
second ty str = first ty
                      (Structor { ductive = streamDuc
                                , parameters = [ty]
                                , num = 1 } :@: str)

destructorTests :: Spec
destructorTests = do
  it "Type checks head on constant stream of units to unit type" $
    shouldCheckWithDefs [streamD] (first UnitType unitStreamExpr)
      ([], UnitType)
  it "Evaluates head on constant stream of units to unit expression" $
    shouldEvalWithDefs [streamD] (first UnitType unitStreamExpr)
      UnitExpr
  it "Type checks head on tail of constant stream of units to unit type" $
    shouldCheckWithDefs [streamD] (second UnitType unitStreamExpr)
      ([], UnitType)
  it "Evaluates head on tail of constant stream of units to unit expression" $
    shouldEvalWithDefs [streamD] (second UnitType unitStreamExpr)
      UnitExpr
  it "Type checks head on constant stream of zeros to nat" $
    shouldCheckWithDefs [zeroDR, streamD] (first (GlobalTypeVar "Nat" []) zeroStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on constant stream of zeros to zero" $
    shouldEvalWithDefs [zeroDR, streamD] (first (GlobalTypeVar "Nat" []) zeroStreamExpr)
      zeroExpr
  it "Type checks head on tail of constant stream of zeros to nat" $
    shouldCheckWithDefs [zeroDR, streamD] (second (GlobalTypeVar "Nat" []) zeroStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on tail of constant stream of zeros to zero" $
    shouldEvalWithDefs [zeroDR, streamD] (second (GlobalTypeVar "Nat" []) zeroStreamExpr)
      zeroExpr
  it "Type checks head on constant stream of ones to nat" $
    shouldCheckWithDefs [oneDR, streamD] (first (GlobalTypeVar "Nat" []) oneStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on constant stream of ones to one" $
    shouldEvalWithDefs [oneDR, streamD] (first (GlobalTypeVar "Nat" []) oneStreamExpr)
      oneExprI
  it "Type checks head on tail of constant stream of ones to nat" $
    shouldCheckWithDefs [oneDR, streamD] (second (GlobalTypeVar "Nat" []) oneStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on tail of constant stream of ones to one" $
    shouldEvalWithDefs [oneDR, streamD] (second (GlobalTypeVar "Nat" []) oneStreamExpr)
      oneExprI
  it "Type checks head on constant stream of twos to nat" $
    shouldCheckWithDefs [twoDR, streamD] (first (GlobalTypeVar "Nat" []) twoStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on constant stream of twos to two" $
    shouldEvalWithDefs [twoDR, streamD] (first (GlobalTypeVar "Nat" []) twoStreamExpr)
      twoExprI
  it "Type checks head on tail of constant stream of twos to nat" $
    shouldCheckWithDefs [twoDR, streamD] (second (GlobalTypeVar "Nat" []) twoStreamExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates head on tail of constant stream of twos to two" $
    shouldEvalWithDefs [twoDR, streamD] (second (GlobalTypeVar "Nat" []) twoStreamExpr)
      twoExprI
