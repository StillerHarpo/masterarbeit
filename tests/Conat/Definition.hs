{-# language OverloadedStrings#-}
module Conat.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Maybe.Definition

conatD :: Text
conatD = T.unlines
  [ "codata Conat : Set where"
  , "  Prev  : Conat -> Maybe<Conat>"
  ]

conatDR :: Text
conatDR = T.unlines [maybeD, conatD]

conatDuc :: Ductive
conatDuc = Ductive { gamma = []
                   , sigmas = [[]]
                   , as = [ GlobalTypeVar "Maybe" [LocalTypeVar 0 "Conat"]]
                   , gamma1s = [[]]
                   , nameDuc = "Conat"
                   , strNames = ["Prev"]}

conatExpr = Coin conatDuc

conatTest :: Spec
conatTest =
  it "Parses the definition of Conat" $
    shouldParseWithDefs [maybeD] conatD
      [ TypeDef { name = "Conat"
                , parameterCtx = []
                , typeExpr = conatExpr
                , kind = Nothing}]


