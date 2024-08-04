module Main (main) where

import Spec.MerkelizedValidatorSpec
import Spec.MultiUTxOIndexerOneToManySpec
import Spec.MultiUTxOIndexerSpec
import Spec.SingularUTxOIndexerOneToManySpec
import Spec.SingularUTxOIndexerSpec
import Spec.StakeValidatorSpec
import Spec.TxLevelMinterSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ Spec.MerkelizedValidatorSpec.spendUnitTest
      , Spec.MerkelizedValidatorSpec.withdrawUnitTest
      , Spec.MerkelizedValidatorSpec.propertyTest
      , Spec.StakeValidatorSpec.unitTest
      , Spec.StakeValidatorSpec.propertyTest
      , Spec.TxLevelMinterSpec.unitTest
      , Spec.TxLevelMinterSpec.propertyTest
      , Spec.SingularUTxOIndexerSpec.unitTest
      , Spec.SingularUTxOIndexerSpec.propertyTest
      , Spec.SingularUTxOIndexerOneToManySpec.unitTest
      , Spec.SingularUTxOIndexerOneToManySpec.propertyTest
      , Spec.MultiUTxOIndexerSpec.unitTest
      , Spec.MultiUTxOIndexerSpec.propertyTest
      , Spec.MultiUTxOIndexerOneToManySpec.unitTest
      , Spec.MultiUTxOIndexerOneToManySpec.propertyTest
      ]
