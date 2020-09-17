{-# language OverloadedStrings#-}
module Bool.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib


boolD :: Text
boolD = T.unlines
  [ "data Bool : Set where"
  , "  True : Unit -> Bool"
  , "  False  : Unit -> Bool"
  ]

boolDuc :: OpenDuctive
boolDuc = OpenDuctive { gamma = []
                      , inOrCoin = IsIn
                      , parameterCtx = []
                      , strDefs = [ StrDef { sigma = []
                                           , a = UnitType
                                           , gamma1 = []
                                           , strName = "True"}
                                  , StrDef { sigma = []
                                           , a = UnitType
                                           , gamma1 = []
                                           , strName = "False"}]
                      , nameDuc = "Bool" }

boolExpr :: TypeExpr
boolExpr = Ductive { openDuctive = boolDuc
                   , parametersTyExpr = []}

boolTest :: Spec
boolTest =
  it "Parses the definition of Bool" $
    shouldParseWithDefs [] boolD
      [ TypeDef boolDuc ]
