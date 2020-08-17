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

pairDuc :: Ductive
pairDuc = Ductive { gamma = []
                  , sigmas = [[],[]]
                  , as = [ LocalTypeVar 2 "A"
                         , LocalTypeVar 1 "B"]
                  , gamma1s = [[],[]]
                  , nameDuc = "Pair"
                  , strNames = [ "First"
                               , "Second"]}

pairExpr :: TypeExpr
pairExpr = Coin pairDuc

pairTest :: Spec
pairTest =
  it "parses definition" $
      shouldParseWithDefs [] pairD
        [ TypeDef { name = "Pair"
                  , parameterCtx = [[],[]]
                  , kind = Nothing
                  , typeExpr = pairExpr}]

