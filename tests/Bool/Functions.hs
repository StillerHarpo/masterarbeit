{-# language OverloadedStrings#-}
module Bool.Functions where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Bool.Definition
import Bool.Examples
import Packed.Definition
import Pair.Definition
import Pair.Examples

idD :: Text
idD = T.unlines
  [ "id = rec Bool to Bool where"
  , "       { True x = True @ x"
  , "       ; False x = False @ x }"
  ]

idExpr :: Expr
idExpr = Rec { fromRec = boolDuc
             , toRec = GlobalTypeVar "Bool" []
             , matches = [ Constructor boolDuc 0 :@: LocalExprVar 0 "x"
                         , Constructor boolDuc 1 :@: LocalExprVar 0 "x"]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [boolD] idD
      [ ExprDef { name = "id"
                , expr = idExpr
                , ty = Nothing}]
  it "Type checks id to (Bool) -> Bool" $
    shouldCheckWithDefs [boolD] idExpr
      ([boolExpr], GlobalTypeVar "Bool" [])
  it "Type checks id on true to Bool" $
    shouldCheckWithDefs [boolD] (idExpr :@: trueExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates id on true to true" $
    shouldEvalWithDefs [boolD] (idExpr :@: trueExpr)
      trueExpr
  it "Type checks id on false to Bool" $
    shouldCheckWithDefs [boolD] (idExpr :@: falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates id on false to false" $
    shouldEvalWithDefs [boolD] (idExpr :@: falseExpr)
      falseExpr

negD :: Text
negD = T.unlines
  [ "neg = rec Bool to Bool where"
  , "       { True x = False @ x"
  , "       ; False x = True @ x }"
  ]

negExpr :: Expr
negExpr = Rec { fromRec = boolDuc
              , toRec = GlobalTypeVar "Bool" []
              , matches = [ Constructor boolDuc 1 :@: LocalExprVar 0 "x"
                          , Constructor boolDuc 0 :@: LocalExprVar 0 "x"]}

negTests :: Spec
negTests = do
  it "Parses neg" $
    shouldParseWithDefs [boolD] negD
      [ ExprDef { name = "neg"
                , expr = negExpr
                , ty = Nothing}]
  it "Type checks neg to (Bool) -> Bool" $
    shouldCheckWithDefs [boolD] negExpr
      ([boolExpr], GlobalTypeVar "Bool" [])
  it "Type checks neg on true to Bool" $
    shouldCheckWithDefs [boolD] (negExpr :@: trueExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates neg on true to false" $
    shouldEvalWithDefs [boolD] (negExpr :@: trueExpr)
      falseExpr
  it "Type checks neg on false to Bool" $
    shouldCheckWithDefs [boolD] (negExpr :@: falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates neg on false to true" $
    shouldEvalWithDefs [boolD] (negExpr :@: falseExpr)
      trueExpr

orD :: Text
orD = T.unlines
  [ "or = rec<Pair<Bool,Bool>> Packed to Bool where"
  , "       Pack x = (rec Bool to Bool where"
  , "                   { True u = True @ u "
  , "                   ; False u = Second<Bool,Bool> @ x}) @ (First<Bool,Bool> @ x)" ]

orExpr :: Expr
orExpr = WithParameters [GlobalTypeVar "Pair" [ GlobalTypeVar "Bool" []
                                              , GlobalTypeVar "Bool" []]] $
  Rec { fromRec = packedDucA
      , toRec = GlobalTypeVar "Bool" []
      , matches =
          [Rec { fromRec = boolDuc
               , toRec = GlobalTypeVar "Bool" []
               , matches = [ Constructor boolDuc 0 :@: LocalExprVar 0 "u"
                           , WithParameters [ GlobalTypeVar "Bool" []
                                            , GlobalTypeVar "Bool" [] ]
                                            (Destructor pairDucAB 1)
                             :@: LocalExprVar 1 "x"]}
           :@: (WithParameters [ GlobalTypeVar "Bool" []
                               , GlobalTypeVar "Bool" []]
                               (Destructor pairDucAB 0)
               :@: LocalExprVar 0 "x")]}

orTests :: Spec
orTests = do
  it "Parses or" $
    shouldParseWithDefs [packedD, pairD, boolD] orD
      [ ExprDef { name = "or"
                , expr = orExpr
                , ty = Nothing}]
  it "Type checks or to (Bool x Bool) -> Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] orExpr
      ([packedExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Bool" []
                                         , GlobalTypeVar "Bool" []])]
      , GlobalTypeVar "Bool" [])
  let boolPair x y =
        Constructor (packedDuc (GlobalTypeVar "Pair"
                                               [ GlobalTypeVar "Bool" []
                                               , GlobalTypeVar "Bool" []]))
                    0
         :@: mkPairExpr (GlobalTypeVar "Bool" []) (GlobalTypeVar "Bool" []) x y
  it "Type checks or on true x true to Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] (orExpr
                                                 :@: boolPair trueExpr trueExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates or on true x true to true" $
    shouldEvalWithDefs [packedD, pairD, boolD] (orExpr
                                                :@: boolPair trueExpr trueExpr)
      trueExpr
  it "Type checks or on true x false to Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] (orExpr
                                                 :@: boolPair trueExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates or on true x false to true" $
    shouldEvalWithDefs [packedD, pairD, boolD] (orExpr
                                                :@: boolPair trueExpr falseExpr)
      trueExpr
  it "Type checks or on false x true to Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] (orExpr
                                                 :@: boolPair falseExpr trueExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates or on false x true to true" $
    shouldEvalWithDefs [packedD, pairD, boolD] (orExpr
                                                :@: boolPair falseExpr trueExpr)
      trueExpr
  it "Type checks or on false x false to Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] (orExpr
                                                 :@: boolPair falseExpr falseExpr)
      ([], GlobalTypeVar "Bool" [])
  it "Evaluates or on false x false to false" $
    shouldEvalWithDefs [packedD, pairD, boolD] (orExpr
                                                :@: boolPair falseExpr falseExpr)
      falseExpr


