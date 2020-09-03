{-# language OverloadedStrings#-}
module Maybe.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib


maybeD :: Text
maybeD = T.unlines
  [ "data Maybe<A : Set> : Set where"
  , "  Nothing : Unit -> Maybe"
  , "  Just  : A -> Maybe"
  ]

maybeDuc :: TypeExpr -> Ductive
maybeDuc x = Ductive { gamma = []
                     , sigmas = [[],[]]
                     , as = [ UnitType, x]
                     , gamma1s = [[],[]]
                     , nameDuc = "Maybe"
                     , strNames = ["Nothing", "Just"]}

maybeDucA :: Ductive
maybeDucA = maybeDuc (Parameter 0 "A")

maybeExpr :: TypeExpr -> TypeExpr
maybeExpr = In . maybeDuc

maybeExprA :: TypeExpr
maybeExprA = In maybeDucA

maybeTest :: Spec
maybeTest =
  it "Parses the definition of Maybe" $
    shouldParseWithDefs [] maybeD
      [ TypeDef { name = "Maybe"
                , parameterCtx = [[]]
                , typeExpr = maybeExprA
                , kind = Nothing}]


