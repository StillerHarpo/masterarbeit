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
zeroExpr = Iter { motive = UnitType
                , ductive = conatDuc
                , parameters = []
                , matches = [(["x"], Structor { ductive = maybeDuc
                                              , parameters = [UnitType]
                                              , num = 0}
                                     :@: UnitExpr)]} :@: UnitExpr
oneExpr = Iter { motive = GlobalTypeVar "Conat" []
               , ductive = conatDuc
               , parameters = []
               , matches = [( ["x"]
                            , Structor { ductive = maybeDuc
                                       , parameters =
                                           [GlobalTypeVar "Conat" []]
                                       , num = 1}
                             :@: GlobalExprVar "zero" [] [])]}
         :@: GlobalExprVar "zero" [] []
twoExpr = Iter { motive = GlobalTypeVar "Conat" []
               , ductive = conatDuc
               , parameters = []
               , matches = [( ["x"]
                            , Structor { ductive = maybeDuc
                                       , parameters =
                                           [GlobalTypeVar "Conat" []]
                                       , num = 1}
                             :@: GlobalExprVar "one" [] [])]}
          :@: GlobalExprVar "one" [] []
infinityExpr = Iter { motive = GlobalTypeVar "Conat" []
                    , ductive = conatDuc
                    , parameters = []
                    , matches = [( ["x"]
                                 , Structor { ductive = maybeDuc
                                            , parameters =
                                                [GlobalTypeVar "Conat" []]
                                            , num = 1}
                                 :@: LocalExprVar 0 False "x")]}
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
