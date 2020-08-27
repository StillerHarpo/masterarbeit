module Conat where

import Test.Hspec

import Conat.Definition
import Conat.Examples
import Conat.Functions

tests :: Spec
tests = describe "Conats:" $ do
  conatTest
  conatExTests
  isZeroTests
