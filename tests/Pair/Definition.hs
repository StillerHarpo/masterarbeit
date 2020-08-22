{-# language OverloadedStrings#-}
module Pair.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib

pairD :: Text
pairD = T.unlines
  [ "codata Pair<A : Set,B : Set> : Set where"
  , "  First : Pair -> A"
  , "  Second : Pair -> B"
  ]

pairDuc :: TypeExpr -> TypeExpr -> Ductive
pairDuc x y = Ductive { gamma = []
                      , sigmas = [[],[]]
                      , as = [ x , y]
                      , gamma1s = [[],[]]
                      , nameDuc = "Pair"
                      , strNames = [ "First" , "Second"]}

pairDucAB :: Ductive
pairDucAB = pairDuc (LocalTypeVar 2 "A") (LocalTypeVar 1 "B")

pairExpr :: TypeExpr -> TypeExpr -> TypeExpr
pairExpr x y = Coin $ pairDuc x y

pairExprAB :: TypeExpr
pairExprAB = Coin pairDucAB

pairTest :: Spec
pairTest =
  it "parses definition" $
      shouldParseWithDefs [] pairD
        [ TypeDef { name = "Pair"
                  , parameterCtx = [[],[]]
                  , kind = Nothing
                  , typeExpr = pairExprAB}]

