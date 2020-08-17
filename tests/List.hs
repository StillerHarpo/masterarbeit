{-# language OverloadedStrings#-}
module List where

import Test.Hspec

import List.Definition
import List.Examples
import List.Functions

tests :: Spec
tests = describe "Lists:" $ do
  listTest
  listExTest
  lengthTest

