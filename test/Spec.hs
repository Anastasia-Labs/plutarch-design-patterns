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
      "Unit Test Group"
      [ Spec.MerkelizedValidatorSpec.spendUnitTest
      , Spec.MerkelizedValidatorSpec.withdrawUnitTest
      , Spec.StakeValidatorSpec.unitTest
      , Spec.TxLevelMinterSpec.unitTest
      , Spec.SingularUTxOIndexerSpec.unitTest
      , Spec.SingularUTxOIndexerOneToManySpec.unitTest
      , Spec.MultiUTxOIndexerSpec.unitTest
      , Spec.MultiUTxOIndexerOneToManySpec.unitTest
      ]
