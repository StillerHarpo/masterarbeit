module Packed where

import Test.Hspec

import Packed.Definition
import Packed.Examples
import Packed.Functions

tests :: Spec
tests = describe "Packed:" $ do
  packedTest
  packedExTests
  idTests
