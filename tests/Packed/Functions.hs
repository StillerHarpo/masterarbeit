{-# language OverloadedStrings#-}
module Packed.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Packed.Definition
import Packed.Examples

idD :: Text -> Text
idD ty = T.unlines
  [ "id = rec<" <> ty <> "> Packed to Packed<" <> ty <> "> where"
  , "       { Pack x = Pack<" <> ty <> "> @ x }"
  ]

idExpr :: TypeExpr -> Expr
idExpr ty =
  Iter { ductive = packedDuc
       , parameters = [ty]
       , motive = GlobalTypeVar "Packed" [ty]
       , matches = [(["x"], packExpr ty :@: LocalExprVar 0 False "x")]}

idTests :: Spec
idTests = do
  it "Parses id<Unit>" $
    shouldParseWithDefs [packedD] (idD "Unit")
      [ ExprDef { name = "id"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = idExpr UnitType
                , ty = Nothing}]
  it "Type checks id<Unit> to (Packed<Unit>) -> Packed<Unit>" $
    shouldCheckWithDefs [packedD] (idExpr UnitType)
      ([packedExpr UnitType], GlobalTypeVar "Packed" [UnitType])
  it "Type checks id on packed () to Packed<Unit>" $
    shouldCheckWithDefs [packedD] (idExpr UnitType :@: packedEx1Expr)
      ([], GlobalTypeVar "Packed" [UnitType])
  it "Evaluates id on packed () to packed ()" $
    shouldEvalWithDefs [packedD] (idExpr UnitType :@: packedEx1Expr)
      packedEx1Expr
