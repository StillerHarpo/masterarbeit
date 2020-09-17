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
  , "       { Nothing x = Nothing<Unit> @ ()"
  , "       ; Just x = Just<Unit> @ x }"
  ]

idExpr :: Expr
idExpr =
  Iter { ductive = maybeDuc
       , parameters = [UnitType]
       , motive = GlobalTypeVar "Maybe" [UnitType]
       , matches = [ nothingExpr UnitType
                   , justExpr UnitType :@: LocalExprVar 0 False "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [maybeD] idD
      [ ExprDef { name = "id"
                , tyParameterCtx = []
                , exprParameterCtx = []
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
      maybeEx1Expr
  it "Type checks id on Just () to Maybe<Unit>" $
    shouldCheckWithDefs [maybeD] (idExpr :@: maybeEx2Expr)
      ([], GlobalTypeVar "Maybe" [UnitType])
  it "Evaluates id on Just () to Just ()" $
    shouldEvalWithDefs [maybeD] (idExpr :@: maybeEx2Expr)
      maybeEx2Expr
