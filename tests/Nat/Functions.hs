{-# language OverloadedStrings#-}
module Nat.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples

idD :: Text
idD = T.unlines
  [ "id = rec Nat to Nat where"
  , "       { Zero x = Zero @ x"
  , "       ; Suc x = Suc @ x }"
  ]

idExpr :: Expr
idExpr = Rec { fromRec = natDuc
             , toRec = GlobalTypeVar "Nat" []
             , matches = [ Constructor natDuc 0 :@: LocalExprVar 0 "x"
                         , Constructor natDuc 1 :@: LocalExprVar 0 "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [natD] idD
      [ ExprDef { name = "id"
                , expr = idExpr
                , ty = Nothing}]
  it "Type checks id to (Nat) -> Nat" $
    shouldCheckWithDefs [natD] idExpr
      ([natExpr], GlobalTypeVar "Nat" [])
  it "Type checks id on zero to Nat" $
    shouldCheckWithDefs [natD] (idExpr :@: zeroExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates id on zero to zero" $
    shouldEvalWithDefs [natD] (idExpr :@: zeroExpr)
      zeroExpr
  it "Type checks id on one to Nat" $
    shouldCheckWithDefs [zeroDR] (idExpr :@: oneExpr)
      ([], GlobalTypeVar "Nat" [])
  it "Evaluates id on one to inlined one" $
    shouldEvalWithDefs [zeroDR] (idExpr :@: oneExpr)
      oneExprI
  it "Evaluates id on inlined one to inlined one" $
    shouldEvalWithDefs [natD] (idExpr :@: oneExprI)
      oneExprI
  it "Evaluates id on two to inlined two" $
    shouldEvalWithDefs [oneDR] (idExpr :@: twoExpr)
      twoExprI
  it "Evaluates id on inlined two to inlined two" $
    shouldEvalWithDefs [natD] (idExpr :@: twoExprI)
      twoExprI
