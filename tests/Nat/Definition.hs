{-# language OverloadedStrings#-}
module Nat.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib


natD :: Text
natD = T.unlines
  [ "data Nat : Set where"
  , "  Zero : Unit -> Nat"
  , "  Suc  : Nat -> Nat"
  ]

natDuc :: Ductive
natDuc = Ductive { gamma = []
                 , strDefs = [ StrDef { sigma = []
                                      , a = UnitType
                                      , gamma1 = []
                                      , strName = "Zero"}
                             , StrDef { sigma = []
                                      , a = LocalTypeVar 0 "Nat"
                                      , gamma1 = []
                                      , strName = "Suc"}]
                 , nameDuc = "Nat"}

natExpr = In natDuc

natTest :: Spec
natTest =
  it "Parses the definition of Nat" $
    shouldParseWithDefs [] natD
      [ TypeDef { name = "Nat"
                , parameterCtx = []
                , typeExpr = natExpr
                , kind = Nothing}]


