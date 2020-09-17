{-# language OverloadedStrings#-}
module Nat.Examples where

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    (forAll, hedgehog)

import qualified Data.Text              as T
import           Data.Text              (Text)

import           AbstractSyntaxTree

import           Lib
import           Nat.Definition

zeroD, zeroDR, oneD, oneDR, twoD, twoDR :: Text
zeroD = "zero = Zero @ ()"
zeroDR = T.unlines [natD, zeroD]
oneD = "one = Suc @ zero"
oneDR = T.unlines [zeroDR, oneD]
twoD = "two = Suc @ one"
twoDR = T.unlines [oneDR, twoD]

zeroExpr, oneExpr, oneExprI, twoExpr, twoExprI :: Expr
zeroExpr = Structor { ductive = natDuc
                    , parameters = []
                    , num = 0 }
           :@: UnitExpr
oneExpr = Structor { ductive = natDuc
                   , parameters = []
                   , num = 1 }
          :@: GlobalExprVar "zero" [] []
oneExprI = Structor { ductive = natDuc
                    , parameters = []
                    , num = 1 }
           :@: zeroExpr
twoExpr = Structor { ductive = natDuc
                    , parameters = []
                    , num = 1 }
          :@: GlobalExprVar "one" [] []
twoExprI = Structor { ductive = natDuc
                    , parameters = []
                    , num = 1 }
           :@: oneExprI

genNatExpr :: Int -> Expr
genNatExpr 0 = zeroExpr
genNatExpr n = Structor { ductive = natDuc
                        , parameters = []
                        , num = 1 }
               :@: genNatExpr (n - 1)

natExTests :: Spec
natExTests = do
  it "Parses zero" $
    shouldParseWithDefs [natD] zeroD
      [ ExprDef { name = "zero"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = zeroExpr
                , ty = Nothing}]
  it "Type checks zero to nat" $
    shouldCheckWithDefs [natD] zeroExpr
      ([], natExpr)
  it "Parses one" $
    shouldParseWithDefs [zeroDR] oneD
      [ ExprDef { name = "one"
                , tyParameterCtx = []
                , exprParameterCtx = []
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
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = twoExpr
                , ty = Nothing}]
  it "Type checks two to nat" $
    shouldCheckWithDefs [oneDR] twoExpr
      ([], natExpr)
  it "Type checks inlined two to nat" $
    shouldCheckWithDefs [natD] twoExprI
      ([], natExpr)
  it "Type checks 193 to nat" $
    shouldCheckWithDefs [natD] (genNatExpr 193)
      ([], natExpr)
  it "Type checks any nat to nat" $ hedgehog $ do
      n <- forAll $ Gen.integral (Range.linear 0 1000)
      shouldCheckWithDefsP [natD] (genNatExpr n)
        ([], natExpr)
