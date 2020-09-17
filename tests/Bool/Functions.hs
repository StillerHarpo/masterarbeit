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
  , "       { True x = True @ ()"
  , "       ; False x = False @ () }"
  ]

idExpr :: Expr
idExpr = Iter { ductive = boolDuc
              , parameters = []
              , motive = GlobalTypeVar "Bool" []
              , matches = [ trueExpr, falseExpr ]}

idTests :: Spec
idTests = do
  it "Parses id" $
    shouldParseWithDefs [boolD] idD
      [ ExprDef { name = "id"
                , tyParameterCtx = []
                , exprParameterCtx = []
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
  , "       { True x = False @ ()"
  , "       ; False x = True @ ()}"
  ]

negExpr :: Expr
negExpr = Iter { ductive = boolDuc
               , parameters = []
               , motive = GlobalTypeVar "Bool" []
               , matches = [ falseExpr, trueExpr ]}

negTests :: Spec
negTests = do
  it "Parses neg" $
    shouldParseWithDefs [boolD] negD
      [ ExprDef { name = "neg"
                , tyParameterCtx = []
                , exprParameterCtx = []
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
  , "                   { True u = True @ () "
  , "                   ; False u = Second<Bool,Bool> @ x}) @ (First<Bool,Bool> @ x)" ]

orExpr :: Expr
orExpr =
  Iter { ductive = packedDuc
       , parameters = [GlobalTypeVar "Pair" [ GlobalTypeVar "Bool" []
                                            , GlobalTypeVar "Bool" []]]
       , motive = GlobalTypeVar "Bool" []
       , matches =
          [Iter { ductive = boolDuc
                , parameters = []
                , motive = GlobalTypeVar "Bool" []
                , matches = [ trueExpr
                            , sndExpr (GlobalTypeVar "Bool" [])
                                      (GlobalTypeVar "Bool" [])
                              :@: LocalExprVar 1 False "x"]}
           :@: (fstExpr (GlobalTypeVar "Bool" [])
                        (GlobalTypeVar "Bool" [])
               :@: LocalExprVar 0 False "x")]}

orTests :: Spec
orTests = do
  it "Parses or" $
    shouldParseWithDefs [packedD, pairD, boolD] orD
      [ ExprDef { name = "or"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = orExpr
                , ty = Nothing}]
  it "Type checks or to (Bool x Bool) -> Bool" $
    shouldCheckWithDefs [packedD, pairD, boolD] orExpr
      ([packedExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Bool" []
                                         , GlobalTypeVar "Bool" []])]
      , GlobalTypeVar "Bool" [])
  let boolPair x y =
        packExpr (GlobalTypeVar "Pair" [ GlobalTypeVar "Bool" []
                                        , GlobalTypeVar "Bool" []])
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


