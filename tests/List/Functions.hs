{-# language OverloadedStrings#-}
module List.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
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
  Rec { fromRec = listDuc
      , toRec = GlobalTypeVar "Nat" []
      , matches = [ Constructor natDuc 0 :@: UnitExpr
                  , Constructor natDuc 1
                    :@: (WithParameters [UnitType, GlobalTypeVar "Nat" []]
                                        (Destructor pairDuc 1)
                         :@: LocalExprVar 0 "n")]}

lengthTest :: Spec
lengthTest = do
  it "Parses length" $
    shouldParseWithDefs [natD, pairD, listD] lengthD
      [ ExprDef { name = "length"
                , expr = lengthExpr
                , ty = Nothing}]
  it "Type checks length to (List<Unit>) -> Nat" $
    shouldCheckWithDefs [natD, pairD, listD] lengthExpr
      ([GlobalTypeVar "List" [UnitType]], GlobalTypeVar "Nat" [])
