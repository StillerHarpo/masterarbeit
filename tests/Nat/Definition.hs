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

natDuc :: OpenDuctive
natDuc = OpenDuctive { gamma = []
                     , inOrCoin = IsIn
                     , parameterCtx = []
                     , strDefs = [ StrDef { sigma = []
                                          , a = UnitType
                                          , gamma1 = []
                                          , strName = "Zero"}
                                 , StrDef { sigma = []
                                          , a = LocalTypeVar 0 False "Nat"
                                          , gamma1 = []
                                          , strName = "Suc"}]
                     , nameDuc = "Nat"}

natExpr :: TypeExpr
natExpr = Ductive { openDuctive = natDuc
                  , parametersTyExpr = []}

sucExpr :: Expr
sucExpr = Structor { ductive = natDuc
                   , parameters = []
                   , num = 1}

natTest :: Spec
natTest =
  it "Parses the definition of Nat" $
    shouldParseWithDefs [] natD
      [ TypeDef natDuc ]
