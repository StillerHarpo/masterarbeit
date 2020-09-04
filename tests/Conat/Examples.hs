{-# language OverloadedStrings#-}
module Conat.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Conat.Definition
import Maybe.Definition

zeroD, zeroDR, oneD, oneDR, twoD, twoDR, infinityD, infinityDR :: Text
zeroD = T.unlines
  ["zero = (corec Unit to Conat where"
  ,"          {Prev x = Nothing<Unit> @ ()}) @ ()"]
zeroDR = T.unlines [conatDR, zeroD]
oneD = T.unlines
  ["one = (corec Conat to Conat where"
  ,"          {Prev x = Just<Conat> @ zero}) @ zero"]
oneDR = T.unlines [zeroDR, oneD]
twoD = T.unlines
  ["two = (corec Conat to Conat where"
  ,"          {Prev x = Just<Conat> @ one}) @ one"]
twoDR = T.unlines [oneDR, twoD]
infinityD = T.unlines
  ["infinity = (corec Conat to Conat where"
  ,"             {Prev x = Just<Conat> @ x}) @ zero"]
infinityDR = T.unlines [zeroDR, infinityD]

zeroExpr, oneExpr, twoExpr, infinityExpr :: Expr
zeroExpr = Corec { fromCorec = UnitType
                 , toCorec = conatDuc
                 , matches = [ WithParameters [UnitType]
                                              (Constructor maybeDucA 0)
                               :@: UnitExpr]} :@: UnitExpr
oneExpr = Corec { fromCorec = GlobalTypeVar "Conat" []
                , toCorec = conatDuc
                , matches = [ WithParameters [GlobalTypeVar "Conat" []]
                                             (Constructor maybeDucA 1)
                              :@: GlobalExprVar "zero" [] []]}
          :@: GlobalExprVar "zero" [] []
twoExpr = Corec { fromCorec = GlobalTypeVar "Conat" []
                , toCorec = conatDuc
                , matches = [ WithParameters [GlobalTypeVar "Conat" []]
                                             (Constructor maybeDucA 1)
                              :@: GlobalExprVar "one" [] [] ]}
          :@: GlobalExprVar "one" [] []
infinityExpr = Corec { fromCorec = GlobalTypeVar "Conat" []
                     , toCorec = conatDuc
                     , matches = [ WithParameters [GlobalTypeVar "Conat" []]
                                                  (Constructor maybeDucA 1)
                                   :@: LocalExprVar 0 "x"]}
          :@: GlobalExprVar "zero" [] []

conatExTests :: Spec
conatExTests = do
  it "Parses zero" $
    shouldParseWithDefs [conatDR] zeroD
      [ ExprDef { name = "zero"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = zeroExpr
                , ty = Nothing}]
  it "Type checks zero to conat" $
    shouldCheckWithDefs [conatDR] zeroExpr
      ([], conatExpr)
  it "Parses one" $
    shouldParseWithDefs [zeroDR] oneD
      [ ExprDef { name = "one"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = oneExpr
                , ty = Nothing}]
  it "Type checks one to conat" $
    shouldCheckWithDefs [zeroDR] oneExpr
      ([], conatExpr)
  it "Parses two" $
    shouldParseWithDefs [oneDR] twoD
      [ ExprDef { name = "two"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = twoExpr
                , ty = Nothing}]
  it "Type checks two to conat" $
    shouldCheckWithDefs [oneDR] twoExpr
      ([], conatExpr)
  it "Parses infinity" $
    shouldParseWithDefs [zeroDR] infinityD
      [ ExprDef { name = "infinity"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = infinityExpr
                , ty = Nothing}]
  it "Type checks infinity to conat" $
    shouldCheckWithDefs [zeroDR] infinityExpr
      ([], conatExpr)
