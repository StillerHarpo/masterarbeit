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

streamDuc ::  OpenDuctive
streamDuc =
  OpenDuctive { gamma = []
              , parameterCtx = [[]]
              , inOrCoin = IsCoin
              , strDefs = [ StrDef { sigma = []
                                   , a = Parameter 0 False "A"
                                   , gamma1 = []
                                   , strName = "Head"}
                          , StrDef { sigma = []
                                   , a = LocalTypeVar 0 False "Stream"
                                   , gamma1 = []
                                   , strName = "Tail"}]
              , nameDuc = "Stream" }

streamExpr :: TypeExpr -> TypeExpr
streamExpr x = Ductive { openDuctive = streamDuc
                       , parametersTyExpr = [x] }

streamTest :: Spec
streamTest =
  it "Parses the definition of Stream" $
    shouldParseWithDefs [] streamD
      [ TypeDef streamDuc ]
