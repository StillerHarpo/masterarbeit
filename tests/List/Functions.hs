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
  [ "head = rec<" <> ty <> "> List to Maybe<Unit> where"
  , "         Nil n = Nothing<" <> ty <> "> @ ()"
  , "         Cons n = Just<" <> ty <> "> @ (First<" <> ty <> ",Maybe<" <> ty <> ">> @ n)"]

headExpr :: TypeExpr -> Expr
headExpr ty = WithParameters [ty]
  Rec { fromRec = listDucA
      , toRec = GlobalTypeVar "Maybe" [ty]
      , matches = [ WithParameters [ty] (Constructor maybeDucA 0)
                    :@: UnitExpr
                  , WithParameters [ty] (Constructor maybeDucA 1)
                    :@: (WithParameters [ ty
                                        , GlobalTypeVar "Maybe" [ty]]
                                         (Destructor pairDucAB 0)
                        :@: LocalExprVar 0 "n")]}

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
      (Constructor (maybeDuc UnitType) 0 :@: UnitExpr)
  it "Evaluates head on one element list to Just ()" $
    shouldEvalWithDefs [maybeD, listEx1DR] (headExpr UnitType :@: listEx2Expr)
      (Constructor (maybeDuc UnitType) 1 :@: UnitExpr)
  modifyMaxSuccess (const 10) $ it "Evaluates head on any list" $ hedgehog $ do
      x <- forAll $ Gen.integral (Range.linear 0 5)
      xs <- forAll $ Gen.list (Range.linear 0 5) (Gen.integral (Range.linear 0 5))
      shouldEvalWithDefsP [pairD, natD, listD] (headExpr (GlobalTypeVar "Nat" [])
                                                 :@: genListExpr (GlobalTypeVar "Nat" [])
                                                                 (map genNatExpr (x:xs)))
        (Constructor (maybeDuc (GlobalTypeVar "Nat" [])) 1 :@: genNatExpr x)

appD :: Text -> Text
appD ty = T.unlines
  [ "app = rec<Pair<List<" <> ty <> ">,List<" <> ty <> ">>> Packed to List<" <> ty <> "> where"
  , "        Pack x = (rec<" <> ty <> "> List to List<" <> ty <> "> where"
  , "                    { Nil n = Second<List<" <> ty <> ">,List<" <> ty <> ">> @ x"
  , "                    ; Cons n = Cons<" <> ty <> "> @ n}) @ (First<List<" <> ty <> ">,List<" <> ty <> ">> @ x)"
  ]

appExpr :: TypeExpr -> Expr
appExpr ty = WithParameters [GlobalTypeVar "Pair" [ GlobalTypeVar "List" [ty]
                                                  , GlobalTypeVar "List" [ty]]] $
  Rec { fromRec = packedDucA
      , toRec = GlobalTypeVar "List" [ty]
      , matches =
          [WithParameters [ty]
             (Rec { fromRec = listDucA
                  , toRec = GlobalTypeVar "List" [ty]
                  , matches = [ WithParameters [ GlobalTypeVar "List" [ty]
                                               , GlobalTypeVar "List" [ty] ]
                                               (Destructor pairDucAB 1)
                                :@: LocalExprVar 1 "x"
                               , WithParameters [ty] (Constructor listDucA 1)
                                 :@: LocalExprVar 0 "n"]})
              :@: (WithParameters [ GlobalTypeVar "List" [ty]
                                  , GlobalTypeVar "List" [ty]]
                                  (Destructor pairDucAB 0)
                   :@: LocalExprVar 0 "x")]}

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
        Constructor (packedDuc (GlobalTypeVar "Pair"
                                               [ GlobalTypeVar "List" [ty]
                                               , GlobalTypeVar "List" [ty]]))
                    0
        -}
        WithParameters [GlobalTypeVar "Pair"
                                      [ GlobalTypeVar "List" [ty]
                                      , GlobalTypeVar "List" [ty]]]
                       (Constructor packedDucA 0)
         :@: mkPairExpr (GlobalTypeVar "List" [ty])
                        (GlobalTypeVar "List" [ty])
                        x y
      listPairExD = T.unlines
        [ "app @ (Pack<Pair<List<Unit>,List<Unit>>>"
        , "       @ (corec<List<Unit>,List<Unit>> Unit to Pair where"
        , "            { First x = Nil<Unit> @ ()"
        , "            ; Second x = Nil<Unit> @ () } @ ()))"]
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

