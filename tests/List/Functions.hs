{-# language OverloadedStrings#-}
module List.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Maybe.Definition
import Pair.Definition
import List.Definition
import List.Examples

lengthD :: Text -> Text
lengthD ty = T.unlines
  -- TODO make this generic after implementing it
  [ "length = rec<" <> ty <> "> List to Nat where"
  , "           Nil n = Zero @ ()"
  , "           Cons n = Suc @ (Second<" <> ty <> ",Nat> @ n)"
  ]

lengthExpr :: TypeExpr -> Expr
lengthExpr ty = WithParameters [ty]
  Rec { fromRec = listDucA
      , toRec = GlobalTypeVar "Nat" []
      , matches = [ Constructor natDuc 0 :@: UnitExpr
                  , Constructor natDuc 1
                    :@: (WithParameters [ty, GlobalTypeVar "Nat" []]
                                        (Destructor pairDucAB 1)
                         :@: LocalExprVar 0 "n")]}

lengthTest :: Spec
lengthTest = do
  it "Parses length<Unit>" $
    shouldParseWithDefs [natD, listDR] (lengthD "Unit")
      [ ExprDef { name = "length"
                , expr = lengthExpr UnitType
                , ty = Nothing}]
  it "Type checks length<Unit> to (List<Unit>) -> Nat" $
    shouldCheckWithDefs [natD, listDR] (lengthExpr UnitType)
      ([listExpr UnitType], GlobalTypeVar "Nat" [])
  it "Evaluates length<Unit> on empty list to zero" $
    shouldEvalWithDefs [natD, listDR] (lengthExpr UnitType :@: listEx1Expr)
      zeroExpr
  it "Evaluates length<Unit> on one element list to inlined one" $
    shouldEvalWithDefs [natD, listEx1DR] (lengthExpr UnitType :@: listEx2Expr)
      oneExprI
  it "Parses length<Nat>" $
    shouldParseWithDefs [natD, listDR] (lengthD "Nat")
      [ ExprDef { name = "length"
                , expr = lengthExpr (GlobalTypeVar "Nat" [])
                , ty = Nothing}]
  it "Type checks length<Nat> to (List<Nat>) -> Nat" $
    shouldCheckWithDefs [natD, listDR] (lengthExpr (GlobalTypeVar "Nat" []))
      ([listExpr (GlobalTypeVar "Nat" [])], GlobalTypeVar "Nat" [])
  it "Evaluates length<Nat> on empty list to zero" $
    shouldEvalWithDefs [natD, listDR] (lengthExpr (GlobalTypeVar "Nat" []) :@: listEx3Expr)
      zeroExpr
  it "Evaluates length<Nat> on one element list to inlined one" $
    shouldEvalWithDefs [listEx3DR, zeroD, oneD] (lengthExpr (GlobalTypeVar "Nat" []) :@: listEx4Expr)
      oneExprI
  it "Evaluates length<Nat> on two element list to inlined two" $
    shouldEvalWithDefs [listEx4DR, twoD] (lengthExpr (GlobalTypeVar "Nat" []) :@: listEx5Expr)
      twoExprI


headD :: Text
headD = T.unlines
  [ "head = rec<Unit> List to Maybe<Unit> where"
  , "         Nil n = Nothing<Unit> @ ()"
  , "         Cons n = Just<Unit> @ (First<Unit,Maybe<Unit>> @ n)"]

headExpr :: Expr
headExpr = WithParameters [UnitType]
  Rec { fromRec = listDucA
      , toRec = GlobalTypeVar "Maybe" [UnitType]
      , matches = [ WithParameters [UnitType] (Constructor maybeDucA 0)
                    :@: UnitExpr
                  , WithParameters [UnitType] (Constructor maybeDucA 1)
                    :@: (WithParameters [ UnitType
                                        , GlobalTypeVar "Maybe" [UnitType]]
                                        (Destructor pairDucAB 0)
                        :@: LocalExprVar 0 "n")]}

headTests :: Spec
headTests = do
  it "Parses head" $
    shouldParseWithDefs [maybeD, listDR] headD
      [ ExprDef { name = "head"
                , expr = headExpr
                , ty = Nothing}]
  it "Type checks head to (List<Unit>) -> Maybe<Unit>" $
    shouldCheckWithDefs [maybeD, listDR] headExpr
      ([listExpr UnitType], GlobalTypeVar "Maybe" [UnitType])
  it "Evaluates head on empty list to Nothing" $
    shouldEvalWithDefs [natD, listDR] (headExpr :@: listEx1Expr)
      (Constructor (maybeDuc UnitType) 0 :@: UnitExpr)
  it "Evaluates head on one element list to Just ()" $
    shouldEvalWithDefs [maybeD, listEx1DR] (headExpr :@: listEx2Expr)
      (Constructor (maybeDuc UnitType) 1 :@: UnitExpr)

