{-# language OverloadedStrings#-}
module Stream.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib

streamD :: Text
streamD = T.unlines
  [ "codata Stream<A : Set> : Set where"
  , "  Head : Stream -> A"
  , "  Tail : Stream -> Stream"
  ]

streamDuc :: TypeExpr -> Ductive
streamDuc x = Ductive { gamma = []
                      , sigmas = [[],[]]
                      , as = [ x , LocalTypeVar 0 "Stream"]
                      , gamma1s = [[],[]]
                      , nameDuc = "Stream"
                      , strNames = ["Head", "Tail"]}

streamDucA :: Ductive
streamDucA = streamDuc (Parameter 0 "A")

streamExpr :: TypeExpr -> TypeExpr
streamExpr = Coin . streamDuc

streamExprA :: TypeExpr
streamExprA = Coin streamDucA

streamTest :: Spec
streamTest =
  it "Parses the definition of Stream" $
    shouldParseWithDefs [] streamD
      [ TypeDef { name = "Stream"
                , parameterCtx = [[]]
                , typeExpr = streamExprA
                , kind = Nothing}]

