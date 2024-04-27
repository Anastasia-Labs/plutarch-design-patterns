module Spec.StakeValidatorSpec (
  validator,
) where

import Plutarch.Api.V2 (PScriptContext, PStakeValidator, PStakingCredential, PValidator)
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.StakeValidator qualified as StakeValidator

spend :: Term s PValidator
spend = StakeValidator.spend

withdrawLogic :: Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic =
  plam $ \redData ownValidator txInfo -> unTermCont $ do
    pure $ pconstant ()

withdraw :: Term s PStakeValidator
withdraw = phoistAcyclic $
  plam $ \redeemer ctx -> unTermCont $ do
    return $ StakeValidator.withdraw withdrawLogic redeemer ctx

validator :: Term s PStakeValidator
validator = Multivalidator.multivalidator withdraw spend
