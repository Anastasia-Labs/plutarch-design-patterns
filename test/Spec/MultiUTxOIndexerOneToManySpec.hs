module Spec.MultiUTxOIndexerOneToManySpec (
  validator,
) where

import Plutarch.Api.V2 (PStakeValidator, PValidator)
import Plutarch.MultiUTxOIndexerOneToMany qualified as MultiUTxOIndexerOneToMany
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Spec.Utils qualified as Utils

spend :: Term s PValidator
spend = StakeValidator.spend

withdraw :: Term s PStakeValidator
withdraw = MultiUTxOIndexerOneToMany.withdraw Utils.inputValidator Utils.inputOutputValidator Utils.collectiveOutputValidator

validator :: Term s PValidator
validator = Multivalidator.multivalidator withdraw spend
