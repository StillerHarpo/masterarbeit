{-# language OverloadedStrings#-}
module List.Functions where

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    (forAll, hedgehog, modifyMaxSuccess)

import qualified Data.Text              as T
import           Data.Text(Text)

import           AbstractSyntaxTree
import           TypeChecker
import           Eval

import           Lib
import           Nat.Definition
import           Nat.Examples
import           Packed.Definition
import           Maybe.Definition
import           Pair.Definition
import           Pair.Examples
import           List.Definition
import           List.Examples

lengthD :: Text -> Text
lengthD ty = T.unlines
  -- TODO make this generic after implementing it
  [ "length = rec List<" <> ty <> "> to Nat where"
  , "           Nil n = Zero @ ()"
  , "           Cons n = Suc @ (Second<" <> ty <> ",Nat> @ n)"
  ]

lengthExpr :: TypeExpr -> Expr
lengthExpr ty =
  Iter { ductive = listDuc
       , parameters = [ty]
       , motive = GlobalTypeVar "Nat" []
       , matches = [ (["n"], zeroExpr)
                   , (["n"], sucExpr
                             :@: (sndExpr ty (GlobalTypeVar "Nat" [])
                                  :@: LocalExprVar 0 False "n"))]}

lengthTest :: Spec
lengthTest = do
  it "Parses length<Unit>" $
    shouldParseWithDefs [natD, listDR] (lengthD "Unit")
      [ ExprDef { name = "length"
                , tyParameterCtx = []
                , exprParameterCtx = []
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
                , tyParameterCtx = []
                , exprParameterCtx = []
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
  modifyMaxSuccess (const 10) $ it "Evaluates length on any list" $ hedgehog $ do
      xs <- forAll $ Gen.list (Range.linear 0 5) (Gen.integral (Range.linear 0 5))
      shouldEvalWithDefsP [pairD, natD, listD] (lengthExpr (GlobalTypeVar "Nat" [])
                                                :@: genListExpr (GlobalTypeVar "Nat" []) (map genNatExpr xs))
        (genNatExpr (length xs))



headD :: Text -> Text
headD ty = T.unlines
  [ "head = rec List<" <> ty <> "> to Maybe<Unit> where"
  , "         Nil n = Nothing<" <> ty <> "> @ ()"
  , "         Cons n = Just<" <> ty <> "> @ (First<" <> ty <> ",Maybe<" <> ty <> ">> @ n)"]

headExpr :: TypeExpr -> Expr
headExpr ty =
  Iter { ductive = listDuc
       , parameters = [ty]
       , motive = GlobalTypeVar "Maybe" [ty]
       , matches = [ (["n"], nothingExpr ty)
                   , (["n"], justExpr ty
                           :@: (fstExpr ty (GlobalTypeVar "Maybe" [ty])
                                :@: LocalExprVar 0 False "n"))]}

headTests :: Spec
headTests = do
  it "Parses head" $
    shouldParseWithDefs [maybeD, listDR] (headD "Unit")
      [ ExprDef { name = "head"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = headExpr UnitType
                , ty = Nothing}]
  it "Type checks head to (List<Unit>) -> Maybe<Unit>" $
    shouldCheckWithDefs [maybeD, listDR] (headExpr UnitType)
      ([listExpr UnitType], GlobalTypeVar "Maybe" [UnitType])
  it "Evaluates head on empty list to Nothing" $
    shouldEvalWithDefs [natD, listDR] (headExpr UnitType :@: listEx1Expr)
      (nothingExpr UnitType)
  it "Evaluates head on one element list to Just ()" $
    shouldEvalWithDefs [maybeD, listEx1DR] (headExpr UnitType :@: listEx2Expr)
      (justExpr UnitType :@: UnitExpr)
  modifyMaxSuccess (const 10) $ it "Evaluates head on any list" $ hedgehog $ do
      x <- forAll $ Gen.integral (Range.linear 0 5)
      xs <- forAll $ Gen.list (Range.linear 0 5) (Gen.integral (Range.linear 0 5))
      shouldEvalWithDefsP [pairD, natD, listD] (headExpr (GlobalTypeVar "Nat" [])
                                                 :@: genListExpr (GlobalTypeVar "Nat" [])
                                                                 (map genNatExpr (x:xs)))
        (justExpr (GlobalTypeVar "Nat" []) :@: genNatExpr x)

appD :: Text -> Text
appD ty = T.unlines
  [ "app = rec Packed<Pair<List<" <> ty <> ">,List<" <> ty <> ">>> to List<" <> ty <> "> where"
  , "        Pack x = (rec List<" <> ty <> "> to List<" <> ty <> "> where"
  , "                    { Nil n = Second<List<" <> ty <> ">,List<" <> ty <> ">> @ x"
  , "                    ; Cons n = Cons<" <> ty <> "> @ n}) @ (First<List<" <> ty <> ">,List<" <> ty <> ">> @ x)"
  ]

appExpr :: TypeExpr -> Expr
appExpr ty =
  Iter { ductive = packedDuc
       , motive = GlobalTypeVar "List" [ty]
       , parameters = [GlobalTypeVar "Pair" [ GlobalTypeVar "List" [ty]
                                            , GlobalTypeVar "List" [ty]]]
       , matches =
           [(["x"], Iter { ductive = listDuc
                         , parameters = [ty]
                         , motive = GlobalTypeVar "List" [ty]
                         , matches = [ (["n"], sndExpr (GlobalTypeVar "List" [ty])
                                                       (GlobalTypeVar "List" [ty])
                                               :@: LocalExprVar 1 False "x")
                                     , (["n"], consExpr ty :@: LocalExprVar 0 False "n")]}
                   :@: (fstExpr (GlobalTypeVar "List" [ty])
                                (GlobalTypeVar "List" [ty])
                        :@: LocalExprVar 0 False "x"))]}

appTests :: Spec
appTests = do
  it "Parses app<Unit>" $
    shouldParseWithDefs [packedD, pairD, listD] (appD "Unit")
      [ ExprDef { name = "app"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = appExpr UnitType
                , ty = Nothing}]
  it "Type checks app<Unit> to (List<Unit> x List<Unit>) -> List<Unit>" $
    shouldCheckWithDefs [packedD, pairD, listD] (appExpr UnitType)
      ([packedExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "List" [UnitType]
                                         , GlobalTypeVar "List" [UnitType]])]
      , GlobalTypeVar "List" [UnitType])
  let listPair x y ty =
        {-
        TODO This type checks but doesn't evaluate
        Structor (packedDuc (GlobalTypeVar "Pair"
                                               [ GlobalTypeVar "List" [ty]
                                               , GlobalTypeVar "List" [ty]]))
                    0
        -}
        packExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "List" [ty]
                                       , GlobalTypeVar "List" [ty]])
        :@: mkPairExpr (GlobalTypeVar "List" [ty])
                       (GlobalTypeVar "List" [ty])
                       x y
      listPairExD = T.unlines
        [ "app @ (Pack<Pair<List<Unit>,List<Unit>>>"
        , "       @ (corec Unit to Pair<List<Unit>,List<Unit>> where"
        , "            { First = Nil<Unit> @ ()"
        , "            ; Second = Nil<Unit> @ () } @ ()))"]
  it "Parses app<Unit> on [] x []" $
    shouldParseWithDefs [packedD, pairD, listD, appD "Unit"] listPairExD
      [Expression (GlobalExprVar "app" [] []
                   :@: listPair listEx1Expr listEx1Expr UnitType)]
  it "expr var app evaluates to the same as expr app" $
    shouldEvalSameWithDefs [packedD, pairD, listD, appD "Unit"]
      (GlobalExprVar "app" [] []) (appExpr UnitType)
  it "Type checks app<Unit> on [] x [] to List (as expr var)" $
    shouldCheckWithDefs [packedD, pairD, listD, appD "Unit"]
                        (GlobalExprVar "app" [] []
                         :@: listPair listEx1Expr listEx1Expr UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Type checks app<Unit> on [] x [] to List (as expr)" $
    shouldCheckWithDefs [packedD, pairD, listD, appD "Unit"] (appExpr UnitType
                                                              :@: listPair listEx1Expr listEx1Expr UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Type checks Evaluation of app on [] x [] to zero (as expr var)" $
    shouldRunWithDefs [packedD, pairD, listD, appD "Unit"]
                      (evalInTI (evalExpr (GlobalExprVar "app" [] []
                                           :@: listPair listEx1Expr
                                                        listEx1Expr
                                                        UnitType))
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Type checks Evaluation of app on [] x [] to zero (as expr)" $
    shouldRunWithDefs [packedD, pairD, listD, appD "Unit"]
                      (evalInTI (evalExpr (appExpr UnitType
                                           :@: listPair listEx1Expr
                                                        listEx1Expr
                                                        UnitType))
                       >>= inferTerm)
      ([], listExpr UnitType)
  it "Evaluates length on app on [] x [] to zero" $
    shouldEvalWithDefs [packedD, pairD, listD]
                       (lengthExpr UnitType
                        :@: (appExpr UnitType
                             :@: listPair listEx1Expr
                                          listEx1Expr
                                          UnitType))
      zeroExpr
  it "Type checks app on [] x [()] to List" $
    shouldCheckWithDefs [packedD, listEx1DR]
                        (appExpr UnitType
                         :@: listPair listEx1Expr listEx2Expr UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Evaluates length on app on [] x [()] to one" $
    shouldEvalWithDefs [packedD, natD, listEx1DR]
                       (lengthExpr UnitType
                        :@: (appExpr UnitType
                             :@: listPair listEx1Expr
                                          listEx2Expr
                                          UnitType))
      oneExprI
  it "Type checks app on [()] x [] to List" $
    shouldCheckWithDefs [packedD, listEx1DR]
                        (appExpr UnitType
                         :@: listPair listEx2Expr listEx1Expr UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Evaluates length on app on [()] x [] to one" $
    shouldEvalWithDefs [packedD, natD, listEx1DR]
                       (lengthExpr UnitType
                        :@: (appExpr UnitType
                             :@: listPair listEx2Expr
                                          listEx1Expr
                                          UnitType))
      oneExprI
  it "Type checks app on [()] x [()] to List" $
    shouldCheckWithDefs [packedD, listEx1DR]
                        (appExpr UnitType
                         :@: listPair listEx2Expr listEx2Expr UnitType)
      ([], GlobalTypeVar "List" [UnitType])
  it "Evaluates length on app on [()] x [()] to two" $
    shouldEvalWithDefs [packedD, natD, listEx1DR]
                       (lengthExpr UnitType
                        :@: (appExpr UnitType
                             :@: listPair listEx2Expr
                                          listEx2Expr
                                          UnitType))
      twoExprI

