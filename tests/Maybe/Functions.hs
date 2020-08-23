{-# language OverloadedStrings#-}
module Maybe.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Maybe.Definition
import Maybe.Examples

idD :: Text
idD = T.unlines
  [ "id = rec<Unit> Maybe to Maybe<Unit> where"
  , "       { Nothing x = Nothing<Unit> @ x"
  , "       ; Just x = Just<Unit> @ x }"
  ]

idExpr :: Expr
idExpr = WithParameters [UnitType] $
  Rec { fromRec = maybeDucA
      , toRec = GlobalTypeVar "Maybe" [UnitType]
      , matches = [ WithParameters [UnitType] (Constructor maybeDucA 0)
                    :@: LocalExprVar 0 "x"
                  , WithParameters [UnitType] (Constructor maybeDucA 1)
                    :@: LocalExprVar 0 "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [maybeD] idD
      [ ExprDef { name = "id"
                , expr = idExpr
                , ty = Nothing}]
  it "Type checks id to (Maybe<Unit>) -> Maybe<Unit>" $
    shouldCheckWithDefs [maybeD] idExpr
      ([maybeExpr UnitType], GlobalTypeVar "Maybe" [UnitType])
  it "Type checks id on Nothing to Maybe<Unit>" $
    shouldCheckWithDefs [maybeD] (idExpr :@: maybeEx1Expr)
      ([], GlobalTypeVar "Maybe" [UnitType])
  it "Evaluates id on Nothing to Nothing" $
    shouldEvalWithDefs [maybeD] (idExpr :@: maybeEx1Expr)
      maybeEx1ExprI
  it "Type checks id on Just () to Maybe<Unit>" $
    shouldCheckWithDefs [maybeD] (idExpr :@: maybeEx2Expr)
      ([], GlobalTypeVar "Maybe" [UnitType])
  it "Evaluates id on Just () to Just ()" $
    shouldEvalWithDefs [maybeD] (idExpr :@: maybeEx2Expr)
      maybeEx2ExprI
