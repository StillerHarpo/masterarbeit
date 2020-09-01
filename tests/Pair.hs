module Pair where

import Test.Hspec

import Pair.Definition
import Pair.Examples
import Pair.Functions

tests :: Spec
tests = describe "Pairs:" $ do
  pairTest
  pairExTest
  destructorTests
