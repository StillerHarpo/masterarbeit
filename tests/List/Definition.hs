{-# language OverloadedStrings#-}
module List.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat
import Pair.Definition

listD, listDR :: Text
listD = T.unlines
  [ "data List<A : Set> : Set where"
  , "  Nil : Unit -> List"
  , "  Cons : Pair<A, List> -> List"
  ]
listDR = T.unlines [pairD, listD]

listDuc :: TypeExpr -> Ductive
listDuc x =
  Ductive { gamma = []
          , strDefs =
              [ StrDef { sigma = []
                       , a = UnitType
                       , gamma1 = []
                       , strName = "Nil" }
              , StrDef { sigma = []
                       , a = GlobalTypeVar "Pair" [ x
                                                  , LocalTypeVar 0 "List"]
                       , gamma1 = []
                       , strName = "Cons"}]
          , nameDuc = "List"}

listDucA :: Ductive
listDucA = listDuc (Parameter 0 "A")

listExpr :: TypeExpr -> TypeExpr
listExpr = In . listDuc

listExprA :: TypeExpr
listExprA = In listDucA

listTest :: Spec
listTest =
  it "Parses the definition of List" $
    shouldParseWithDefs [pairD] listD
      [ TypeDef { name = "List"
                , parameterCtx = [[]]
                , typeExpr = listExprA
                , kind = Nothing}]

