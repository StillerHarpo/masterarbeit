{-# language OverloadedStrings#-}
module Bool.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Bool.Definition

trueD, trueDR, falseD, falseDR :: Text
trueD = "true = True @ ()"
trueDR = T.unlines [boolD, trueD]
falseD = "false = False @ ()"
falseDR = T.unlines [boolD, falseD]

trueExpr, falseExpr :: Expr
trueExpr = Constructor boolDuc 0 :@: UnitExpr
falseExpr = Constructor boolDuc 1 :@: UnitExpr

boolExTests :: Spec
boolExTests = do
  it "Parses true" $
    shouldParseWithDefs [boolD] trueD
      [ ExprDef { name = "true"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = trueExpr
                , ty = Nothing}]
  it "Type checks true to bool" $
    shouldCheckWithDefs [boolD] trueExpr
      ([], boolExpr)
  it "Parses false" $
    shouldParseWithDefs [boolD] falseD
      [ ExprDef { name = "false"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = falseExpr
                , ty = Nothing}]
  it "Type checks false to bool" $
    shouldCheckWithDefs [boolD] falseExpr
      ([], boolExpr)
