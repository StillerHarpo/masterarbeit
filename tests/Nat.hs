module Nat where

import Test.Hspec

import Nat.Definition
import Nat.Examples
import Nat.Functions

tests :: Spec
tests = describe "Nats:" $ do
  natTest
  natExTests
  idTests
