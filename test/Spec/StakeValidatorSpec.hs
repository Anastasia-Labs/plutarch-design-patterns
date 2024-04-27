module Spec.StakeValidatorSpec (
  validator,
) where

import Plutarch.Api.V2 (PStakeValidator, PStakingCredential, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator

spend :: Term s PValidator
spend = StakeValidator.spend

withdrawLogic :: Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

withdraw :: Term s PStakeValidator
withdraw = StakeValidator.withdraw withdrawLogic

validator :: Term s PValidator
validator = Multivalidator.multivalidator withdraw spend
