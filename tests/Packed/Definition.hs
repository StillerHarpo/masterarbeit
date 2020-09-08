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
                      , strDefs = [ StrDef { sigma = []
                                           , a = x
                                           , gamma1 = []
                                           , strName = "Pack"}]
                      , nameDuc = "Packed"}

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


