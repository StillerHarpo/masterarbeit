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

maybeDuc ::  OpenDuctive
maybeDuc = OpenDuctive { gamma = []
                       , inOrCoin = IsIn
                       , parameterCtx = [[]]
                       , strDefs = [ StrDef { sigma = []
                                            , a = UnitType
                                            , gamma1 = []
                                            , strName = "Nothing"}
                                   , StrDef { sigma = []
                                            , a = Parameter 0 "A"
                                            , gamma1 = []
                                            , strName = "Just"}]
                       , nameDuc = "Maybe"}

maybeExpr :: TypeExpr -> TypeExpr
maybeExpr x = Ductive { openDuctive = maybeDuc
                      , parametersTyExpr = [x]}

nothingExpr :: TypeExpr -> Expr
nothingExpr x = Structor { ductive = maybeDuc
                         , parameters = [x]
                         , num = 0} :@: UnitExpr

justExpr :: TypeExpr -> Expr
justExpr x = Structor { ductive = maybeDuc
                      , parameters = [x]
                      , num = 1}

maybeTest :: Spec
maybeTest =
  it "Parses the definition of Maybe" $
    shouldParseWithDefs [] maybeD
      [ TypeDef maybeDuc ]


