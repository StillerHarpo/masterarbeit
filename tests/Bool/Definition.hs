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

boolDuc :: Ductive
boolDuc = Ductive { gamma = []
                  , sigmas = [[],[]]
                  , as = [ UnitType, UnitType]
                  , gamma1s = [[],[]]
                  , nameDuc = "Bool"
                  , strNames = ["True", "False"]}

boolExpr = In boolDuc

boolTest :: Spec
boolTest =
  it "Parses the definition of Bool" $
    shouldParseWithDefs [] boolD
      [ TypeDef { name = "Bool"
                , parameterCtx = []
                , typeExpr = boolExpr
                , kind = Nothing}]


