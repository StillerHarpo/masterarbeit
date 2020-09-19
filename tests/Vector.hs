{-# language OverloadedStrings#-}
module Vector where

import Test.Hspec

import Vector.Definition

tests :: Spec
tests = describe "Vectors:" $ do
  pairTest
  vecTest

