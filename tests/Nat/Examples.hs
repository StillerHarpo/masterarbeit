{-# language OverloadedStrings#-}
module Nat.Examples where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition

zeroD, zeroDR, oneD, oneDR, twoD, twoDR :: Text
zeroD = "zero = Zero @ ()"
zeroDR = T.unlines [natD, zeroD]
oneD = "one = Suc @ zero"
oneDR = T.unlines [zeroDR, oneD]
twoD = "two = Suc @ one"
twoDR = T.unlines [oneDR, twoD]
