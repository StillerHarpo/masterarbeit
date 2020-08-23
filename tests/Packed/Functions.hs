{-# language OverloadedStrings#-}
module Packed.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Packed.Definition
import Packed.Examples

idD :: Text
idD = T.unlines
  [ "id = rec<Unit> Packed to Packed<Unit> where"
  , "       { Pack x = Pack<Unit> @ x }"
  ]

idExpr :: Expr
idExpr = WithParameters [UnitType] $
  Rec { fromRec = packedDucA
      , toRec = GlobalTypeVar "Packed" [UnitType]
      , matches = [ WithParameters [UnitType] (Constructor packedDucA 0)
                    :@: LocalExprVar 0 "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [packedD] idD
      [ ExprDef { name = "id"
                , expr = idExpr
                , ty = Nothing}]
  it "Type checks id to (Packed<Unit>) -> Packed<Unit>" $
    shouldCheckWithDefs [packedD] idExpr
      ([packedExpr UnitType], GlobalTypeVar "Packed" [UnitType])
  it "Type checks id on packed () to Packed<Unit>" $
    shouldCheckWithDefs [packedD] (idExpr :@: packedEx1Expr)
      ([], GlobalTypeVar "Packed" [UnitType])
  it "Evaluates id on packed () to packed ()" $
    shouldEvalWithDefs [packedD] (idExpr :@: packedEx1Expr)
      packedEx1ExprI
