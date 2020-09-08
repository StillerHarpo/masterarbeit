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
                  , strDefs = [ StrDef { sigma = []
                                       , a = UnitType
                                       , gamma1 = []
                                       , strName = "True"}
                              , StrDef { sigma = []
                                       , a = UnitType
                                       , gamma1 = []
                                       , strName = "False"}]
                  , nameDuc = "Bool" }

boolExpr = In boolDuc

boolTest :: Spec
boolTest =
  it "Parses the definition of Bool" $
    shouldParseWithDefs [] boolD
      [ TypeDef { name = "Bool"
                , parameterCtx = []
                , typeExpr = boolExpr
                , kind = Nothing}]


