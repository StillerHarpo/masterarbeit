{-# language OverloadedStrings#-}
module Stream.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Stream.Definition
import Nat.Definition
import Nat.Examples

unitStreamD, unitStreamDR, zeroStreamD, zeroStreamDR :: Text
oneStreamD, oneStreamDR, twoStreamD, twoStreamDR :: Text
unitStreamD = T.unlines
  [ "unitStream = (corec<Unit> Unit to Stream where"
  , "                { Head x = ()"
  , "                ; Tail x = ()}) @ ()"]
unitStreamDR = T.unlines [streamD, unitStreamD]
zeroStreamD = T.unlines
  [ "zeroStream = (corec<Nat> Unit to Stream where"
  , "                { Head x = zero"
  , "                ; Tail x = ()}) @ ()"]
zeroStreamDR = T.unlines [streamD, zeroDR, zeroStreamD]
oneStreamD = T.unlines
  [ "oneStream = (corec<Nat> Unit to Stream where"
  , "               { Head x = one"
  , "               ; Tail x = ()}) @ ()"]
oneStreamDR = T.unlines [streamD, oneDR, oneStreamD]
twoStreamD = T.unlines
  [ "twoStream = (corec<Nat> Unit to Stream where"
  , "               { Head x = two"
  , "               ; Tail x = ()}) @ ()"]
twoStreamDR = T.unlines [streamD, twoDR, twoStreamD]


unitStreamExpr, zeroStreamExpr, oneStreamExpr, twoStreamExpr :: Expr
unitStreamExpr = WithParameters [UnitType]
                                (Corec { fromCorec = UnitType
                                       , toCorec = streamDucA
                                       , matches = [UnitExpr, UnitExpr]})
                 :@: UnitExpr
zeroStreamExpr = WithParameters [GlobalTypeVar "Nat" []]
                                (Corec { fromCorec = UnitType
                                       , toCorec = streamDucA
                                       , matches = [ GlobalExprVar "zero" [] []
                                                   , UnitExpr]})
                 :@: UnitExpr
oneStreamExpr = WithParameters [GlobalTypeVar "Nat" []]
                                (Corec { fromCorec = UnitType
                                       , toCorec = streamDucA
                                       , matches = [ GlobalExprVar "one" [] []
                                                   , UnitExpr]})
                 :@: UnitExpr
twoStreamExpr = WithParameters [GlobalTypeVar "Nat" []]
                                (Corec { fromCorec = UnitType
                                       , toCorec = streamDucA
                                       , matches = [ GlobalExprVar "two" [] []
                                                   , UnitExpr]})
                 :@: UnitExpr

streamExTests :: Spec
streamExTests = do
  it "Parses a stream of units" $
    shouldParseWithDefs [streamD] unitStreamD
      [ ExprDef { name = "unitStream"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = unitStreamExpr
                , ty = Nothing}]
  it "Type checks a stream of units to Stream<Unit>" $
    shouldCheckWithDefs [streamD] unitStreamExpr
      ([], streamExpr UnitType)
  it "Parses a stream of zeros" $
    shouldParseWithDefs [zeroDR, streamD] zeroStreamD
      [ ExprDef { name = "zeroStream"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = zeroStreamExpr
                , ty = Nothing}]
  it "Type checks a stream of zeros to Stream<Unit>" $
    shouldCheckWithDefs [zeroDR, streamD] zeroStreamExpr
      ([], streamExpr (GlobalTypeVar "Nat" []))
  it "Parses a stream of ones" $
    shouldParseWithDefs [oneDR, streamD] oneStreamD
      [ ExprDef { name = "oneStream"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = oneStreamExpr
                , ty = Nothing}]
  it "Type checks a stream of ones to Stream<Nat>" $
    shouldCheckWithDefs [oneDR, streamD] oneStreamExpr
      ([], streamExpr (GlobalTypeVar "Nat" []))
  it "Parses a stream of twos" $
    shouldParseWithDefs [twoDR, streamD] twoStreamD
      [ ExprDef { name = "twoStream"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = twoStreamExpr
                , ty = Nothing}]
  it "Type checks a stream of twos to Stream<Nat>" $
    shouldCheckWithDefs [twoDR, streamD] twoStreamExpr
      ([], streamExpr (GlobalTypeVar "Nat" []))

