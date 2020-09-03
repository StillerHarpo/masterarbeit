{-# language OverloadedStrings#-}
module Packed.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib

packedD :: Text
packedD = T.unlines
  [ "data Packed<A : Set> : Set where"
  , "  Pack : A -> Packed"]

packedDuc :: TypeExpr -> Ductive
packedDuc x = Ductive { gamma = []
                      , sigmas = [[]]
                      , as = [ x ]
                      , gamma1s = [[]]
                      , nameDuc = "Packed"
                      , strNames = [ "Pack" ]}

packedDucA :: Ductive
packedDucA = packedDuc (Parameter 0 "A")

packedExpr :: TypeExpr -> TypeExpr
packedExpr = In . packedDuc

packedExprA :: TypeExpr
packedExprA = In packedDucA

packedTest :: Spec
packedTest =
  it "parses definition" $
      shouldParseWithDefs [] packedD
        [ TypeDef { name = "Packed"
                  , parameterCtx = [[]]
                  , kind = Nothing
                  , typeExpr = packedExprA}]


