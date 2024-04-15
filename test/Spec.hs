module Main (main) where

import Spec.MerkelizedValidatorSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ Spec.MerkelizedValidatorSpec.unitTest
      ]
