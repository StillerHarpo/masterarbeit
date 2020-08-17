{-# language OverloadedStrings#-}
module List.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat
import Pair.Definition

listD :: Text
listD = T.unlines
  [ "data List<A : Set> : Set where"
  , "  Nil : Unit -> List"
  , "  Cons : Pair<A, List> -> List"
  ]

listDuc :: Ductive
listDuc = Ductive { gamma = []
                  , sigmas = [[],[]]
                  , as = [ UnitType
                         , GlobalTypeVar "Pair" [ LocalTypeVar 1 "A"
                                                , LocalTypeVar 0 "List"]]
                  , gamma1s = [[],[]]
                  , nameDuc = "List"
                  , strNames = ["Nil", "Cons"]}

listExpr :: TypeExpr
listExpr = In listDuc

listTest :: Spec
listTest =
  it "Parses a List" $
    shouldParseWithDefs [pairD] listD
      [ TypeDef { name = "List"
                , parameterCtx = [[]]
                , typeExpr = listExpr
                , kind = Nothing}]

