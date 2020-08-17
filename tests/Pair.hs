module Pair where

import Test.Hspec

import Pair.Definition
import Pair.Examples

tests :: Spec
tests = describe "Pairs:" $ do
  pairTest
  pairExTest
