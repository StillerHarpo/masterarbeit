{-# language OverloadedStrings#-}
module Packed.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree
import TypeChecker

import Lib

packedD :: Text
packedD = T.unlines
  [ "data Packed<A : Set> : Set where"
  , "  Pack : A -> Packed"]

packedDuc :: OpenDuctive
packedDuc = OpenDuctive { gamma = []
                        , inOrCoin = IsIn
                        , parameterCtx = [[]]
                        , strDefs = [ StrDef { sigma = []
                                             , a = Parameter 0 "A"
                                             , gamma1 = []
                                             , strName = "Pack"}]
                        , nameDuc = "Packed"}

packedExpr :: TypeExpr -> TypeExpr
packedExpr x = Ductive { openDuctive = packedDuc
                       , parametersTyExpr = [x]}

packExpr :: TypeExpr -> Expr
packExpr x = Structor { ductive = packedDuc
                      , parameters = [x]
                      , num = 0}

packedTest :: Spec
packedTest = do
  it "parses definition" $
      shouldParseWithDefs [] packedD
        [ TypeDef packedDuc ]
  it "type checks Definition" $
      shouldRunWithDefs [] (inferType (packedExpr UnitType))
        []
  it "type checks Constructor" $
      shouldCheckWithDefs [] (packExpr UnitType)
        ([UnitType], packedExpr UnitType)


