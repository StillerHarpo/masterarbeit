{-# language OverloadedStrings#-}
module Vector where

import Test.Hspec

import Vector.Definition
import Vector.Examples

tests :: Spec
tests = describe "Vectors:" $ do
  pairTest
  vecTest
  mkPairTests
  nilTests
  consTests
  vecExTests

