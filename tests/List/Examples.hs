{-# language OverloadedStrings#-}
module List.Examples where

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    (forAll, hedgehog)

import qualified Data.Text              as T
import           Data.Text              (Text)

import           AbstractSyntaxTree

import           Lib
import           Nat.Definition
import           Nat.Examples
import           Pair.Definition
import           Pair.Examples
import           List.Definition

listEx1D, listEx1DR, listEx2D, listEx2DR :: Text
listEx3D, listEx3DR, listEx4D, listEx4DR :: Text
listEx5D, listEx5DR :: Text
listEx1D = "list1 = Nil<Unit> @ ()"
listEx1DR = T.unlines [listDR, listEx1D]
listEx2D = "list2 = Cons<Unit> @ "
            <> mkPairD "Unit" "List<Unit>" "()" "list1"
listEx2DR = T.unlines [listEx1DR, listEx2D]
listEx3D = "list3 = Nil<Nat> @ ()"
listEx3DR = T.unlines [natD, listDR, listEx3D]
listEx4D = "list4 = Cons<Nat> @ "
            <> mkPairD "Nat" "List<Nat>" "one" "list3"
listEx4DR = T.unlines [oneDR, listDR, listEx3D, listEx4D]
listEx5D = "list5 = Cons<Nat> @ "
            <> mkPairD "Nat" "List<Nat>" "two" "list4"
listEx5DR = T.unlines [listEx5DR, twoD, listEx5D]

listEx1Expr, listEx2Expr, listEx3Expr, listEx4Expr, listEx5Expr :: Expr
listEx1Expr = nilExpr UnitType
listEx2Expr = consExpr UnitType
              :@: mkPairExpr UnitType
                             (GlobalTypeVar "List" [UnitType])
                             UnitExpr
                             (GlobalExprVar "list1" [] [])
listEx3Expr = nilExpr (GlobalTypeVar "Nat" [])
listEx4Expr = consExpr (GlobalTypeVar "Nat" [])
              :@: mkPairExpr (GlobalTypeVar "Nat" [])
                             (GlobalTypeVar "List"
                                            [GlobalTypeVar "Nat" []])
                             (GlobalExprVar "one" [] [])
                             (GlobalExprVar "list3" [] [])
listEx5Expr = consExpr (GlobalTypeVar "Nat" [])
              :@: mkPairExpr (GlobalTypeVar "Nat" [])
                             (GlobalTypeVar "List"
                                            [GlobalTypeVar "Nat" []])
                             (GlobalExprVar "two" [] [])
                             (GlobalExprVar "list4" [] [])

genListExpr :: TypeExpr -> [Expr] -> Expr
genListExpr ty [] = nilExpr ty
genListExpr ty (x:xs) = consExpr ty
                       :@: mkPairExpr ty (GlobalTypeVar "List" [ty]) x (genListExpr ty xs)

listExTest :: Spec
listExTest = do
  it "Parses a empty list of units" $
    shouldParseWithDefs [listDR] listEx1D
      [ ExprDef { name = "list1"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = listEx1Expr
                , ty = Nothing}]
  it "Type checks a empty list of units to List<Unit>" $
    shouldCheckWithDefs [listDR] listEx1Expr
      ([], listExpr UnitType)
  it "Parses a list with one unit" $
    shouldParseWithDefs [listEx1DR] listEx2D
      [ ExprDef { name = "list2"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = listEx2Expr
                , ty = Nothing}]
  it "Type checks a list with one unit to List<Unit>" $
    shouldCheckWithDefs [listEx1DR] listEx2Expr
      ([], listExpr UnitType)
  it "Parses a empty list of nats" $
    shouldParseWithDefs [natD, listDR] listEx3D
      [ ExprDef { name = "list3"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = listEx3Expr
                , ty = Nothing}]
  it "Type checks a empty list of nats to List<Nat>" $
    shouldCheckWithDefs [natD, listDR] listEx3Expr
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Parses a list with one number one" $
    shouldParseWithDefs [listEx3DR, zeroD, oneD] listEx4D
      [ ExprDef { name = "list4"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = listEx4Expr
                , ty = Nothing}]
  it "Type checks a list with one number one to List<Nat>" $
    shouldCheckWithDefs [listEx3DR, zeroD, oneD] listEx4Expr
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Parses a list with numbers one and two" $
    shouldParseWithDefs [listEx4DR, twoD] listEx5D
      [ ExprDef { name = "list5"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = listEx5Expr
                , ty = Nothing}]
  it "Type checks a list with one number one to List<Nat>" $
    shouldCheckWithDefs [listEx4DR, twoD] listEx5Expr
      ([], listExpr (GlobalTypeVar "Nat" []))
  it "Type checks any list of nats to list of nats" $ hedgehog $ do
      xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.integral (Range.linear 0 100))
      shouldCheckWithDefsP [pairD, natD, listD] (genListExpr (GlobalTypeVar "Nat" []) (map genNatExpr xs))
        ([], listExpr (GlobalTypeVar "Nat" []))


