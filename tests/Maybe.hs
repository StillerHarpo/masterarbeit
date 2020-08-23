module Maybe where

import Test.Hspec

import Maybe.Definition
import Maybe.Examples
import Maybe.Functions

tests :: Spec
tests = describe "Maybe:" $ do
  maybeTest
  maybeExTests
  idTests

