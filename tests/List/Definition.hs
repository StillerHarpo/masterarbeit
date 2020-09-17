{-# language OverloadedStrings#-}
module List.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Pair.Definition

listD, listDR :: Text
listD = T.unlines
  [ "data List<A : Set> : Set where"
  , "  Nil : Unit -> List"
  , "  Cons : Pair<A, List> -> List"
  ]
listDR = T.unlines [pairD, listD]

listDuc :: OpenDuctive
listDuc =
  OpenDuctive { gamma = []
              , inOrCoin = IsIn
              , parameterCtx = [[]]
              , strDefs =
                  [ StrDef { sigma = []
                           , a = UnitType
                           , gamma1 = []
                           , strName = "Nil" }
                  , StrDef { sigma = []
                           , a = GlobalTypeVar "Pair" [ Parameter 0 "A"
                                                      , LocalTypeVar 0 "List"]
                           , gamma1 = []
                           , strName = "Cons"}]
              , nameDuc = "List"}

listExpr :: TypeExpr -> TypeExpr
listExpr x = Ductive { openDuctive = listDuc
                     , parametersTyExpr = [x]}

consExpr :: TypeExpr -> Expr
consExpr x = Structor { ductive = listDuc
                      , parameters = [x]
                      , num = 1}

nilExpr :: TypeExpr -> Expr
nilExpr x = Structor { ductive = listDuc
                     , parameters = [x]
                     , num = 0} :@: UnitExpr

listTest :: Spec
listTest =
  it "Parses the definition of List" $
    shouldParseWithDefs [pairD] listD
      [ TypeDef listDuc ]
