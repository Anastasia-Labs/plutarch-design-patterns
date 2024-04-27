module Spec.MultiUTxOIndexerSpec (
  validator,
) where

import Plutarch.Api.V2 (PStakeValidator, PValidator)
import Plutarch.MultiUTxOIndexer qualified as MultiUTxOIndexer
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Spec.Utils qualified as Utils

spend :: Term s PValidator
spend = StakeValidator.spend

withdraw :: Term s PStakeValidator
withdraw = MultiUTxOIndexer.withdraw Utils.inputOutputValidator

validator :: Term s PValidator
validator = Multivalidator.multivalidator withdraw spend
