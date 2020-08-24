{-# language OverloadedStrings#-}
module Bool where

import Test.Hspec

import Bool.Definition
import Bool.Examples
import Bool.Functions

tests :: Spec
tests = describe "Bools:" $ do
  boolTest
  boolExTests
  idTests
  negTests
  orTests

