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
                      , strDefs = [ StrDef { sigma = []
                                           , a = x
                                           , gamma1 = []
                                           , strName = "First"}
                                  , StrDef { sigma = []
                                           , a = y
                                           , gamma1 = []
                                           , strName = "Second"}]
                      , nameDuc = "Pair" }

pairDucAB :: Ductive
pairDucAB = pairDuc (Parameter 1 "A") (Parameter 0 "B")

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

