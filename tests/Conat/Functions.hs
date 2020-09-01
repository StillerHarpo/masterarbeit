{-# language OverloadedStrings#-}
module Conat.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Conat.Definition
import Conat.Examples
import Maybe.Definition
import Packed.Definition
import Bool.Definition
import Bool.Examples

isZeroD :: Text
isZeroD = T.unlines
  [ "isZero = rec<Conat> Packed to Bool where"
  , "           Pack x = (rec<Conat> Maybe to Bool where"
  , "                       { Nothing x = True @ ()"
  , "                       ; Just x = False @ ()} @ (Prev @ x))"
  ]

isZeroDR :: Text
isZeroDR = T.unlines [packedD, conatDR, boolD, isZeroD]

isZeroExpr :: Expr
isZeroExpr = WithParameters [GlobalTypeVar "Conat" []] $
  Rec { fromRec = packedDucA
      , toRec = GlobalTypeVar "Bool" []
      , matches = [ WithParameters [GlobalTypeVar "Conat" []]
                      (Rec { fromRec = maybeDucA
                           , toRec = GlobalTypeVar "Bool" []
                           , matches = [ trueExpr, falseExpr]})
                   :@: (Destructor conatDuc 0 :@: LocalExprVar 0 "x")]}

isZeroTests :: Spec
isZeroTests = do
  it "Parses isZero" $
    shouldParseWithDefs [packedD, conatDR, boolD] isZeroD
      [ ExprDef { name = "isZero"
                , expr = isZeroExpr
                , ty = Nothing}]
  it "Type checks isZero to (Conat) -> Bool" $
    shouldCheckWithDefs [packedD, conatDR, boolD] isZeroExpr
      ([packedExpr (GlobalTypeVar "Conat" [])], GlobalTypeVar "Bool" [])
  let packConat = Constructor (packedDuc conatExpr) 0
  it "Type checks isZero on zero to Bool" $
    shouldCheckWithDefs [packedD, conatDR, boolD] (isZeroExpr :@: (packConat :@: zeroExpr))
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates isZero on zero to true" $
    shouldEvalWithDefs [packedD, conatDR, boolD] (isZeroExpr :@: (packConat :@: zeroExpr))
      trueExpr
  it "Type checks isZero on one to Bool" $
    shouldCheckWithDefs [packedD, zeroDR, boolD] (isZeroExpr :@: (packConat :@: oneExpr))
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates id on one to false" $
    shouldEvalWithDefs [packedD, zeroDR, boolD] (isZeroExpr :@: (packConat :@: oneExpr))
      falseExpr
  it "Type checks isZero on two to Bool" $
    shouldCheckWithDefs [packedD, oneDR, boolD] (isZeroExpr :@: (packConat :@: twoExpr))
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates isZero on two to false" $
    shouldEvalWithDefs [packedD, oneDR, boolD] (isZeroExpr :@: (packConat :@: twoExpr))
      falseExpr
  it "Type checks isZero on infinity to Bool" $
    shouldCheckWithDefs [packedD, zeroDR, boolD] (isZeroExpr :@: (packConat :@: infinityExpr))
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates isZero on infinity to false" $
    shouldEvalWithDefs [packedD, zeroDR, boolD] (isZeroExpr :@: (packConat :@: infinityExpr))
      falseExpr
