{-# language OverloadedStrings#-}
module List.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples
import Pair.Definition
import List.Definition
import List.Examples

lengthD :: Text
lengthD = T.unlines
  -- TODO make this generic after implementing it
  [ "length = rec<Unit> List to Nat where"
  , "           Nil n = Zero @ ()"
  , "           Cons n = Suc @ (Second<Unit,Nat> @ n)"
  ]

lengthExpr :: Expr
lengthExpr = WithParameters [UnitType]
  Rec { fromRec = listDucA
      , toRec = GlobalTypeVar "Nat" []
      , matches = [ Constructor natDuc 0 :@: UnitExpr
                  , Constructor natDuc 1
                    :@: (WithParameters [UnitType, GlobalTypeVar "Nat" []]
                                        (Destructor pairDucAB 1)
                         :@: LocalExprVar 0 "n")]}

lengthTest :: Spec
lengthTest = do
  it "Parses length" $
    shouldParseWithDefs [natD, listDR] lengthD
      [ ExprDef { name = "length"
                , expr = lengthExpr
                , ty = Nothing}]
  it "Type checks length to (List<Unit>) -> Nat" $
    shouldCheckWithDefs [natD, listDR] lengthExpr
      ([listExpr UnitType], GlobalTypeVar "Nat" [])
  it "Evaluates length on empty list to zero" $
    shouldEvalWithDefs [natD, listDR] (lengthExpr :@: listEx1Expr)
      zeroExpr
  it "Evaluates length on one element list to inlined one" $
    shouldEvalWithDefs [natD, listEx1DR] (lengthExpr :@: listEx2Expr)
      oneExprI
