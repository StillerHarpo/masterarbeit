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

pairDuc :: OpenDuctive
pairDuc = OpenDuctive { gamma = []
                      , inOrCoin = IsCoin
                      , parameterCtx = [[],[]]
                      , strDefs = [ StrDef { sigma = []
                                           , a = Parameter 1 "A"
                                           , gamma1 = []
                                           , strName = "First"}
                                  , StrDef { sigma = []
                                           , a = Parameter 0 "B"
                                           , gamma1 = []
                                           , strName = "Second"}]
                      , nameDuc = "Pair" }

pairExpr :: TypeExpr -> TypeExpr -> TypeExpr
pairExpr x y = Ductive { openDuctive = pairDuc
                       , parametersTyExpr = [x,y]}

fstExpr :: TypeExpr -> TypeExpr -> Expr
fstExpr x y = Structor { ductive = pairDuc
                       , parameters = [x,y]
                       , num = 0}

sndExpr :: TypeExpr -> TypeExpr -> Expr
sndExpr x y = Structor { ductive = pairDuc
                       , parameters = [x,y]
                       , num = 1}

pairTest :: Spec
pairTest =
  it "parses definition" $
      shouldParseWithDefs [] pairD
        [ TypeDef pairDuc ]

