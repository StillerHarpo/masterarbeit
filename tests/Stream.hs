module Stream where

import Test.Hspec

import Stream.Definition
import Stream.Examples
import Stream.Functions

tests :: Spec
tests = describe "Streams:" $ do
  streamTest
  streamExTests
  destructorTests
