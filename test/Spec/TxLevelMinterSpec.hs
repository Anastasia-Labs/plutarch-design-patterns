module Spec.TxLevelMinterSpec (
  validator,
) where

import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.TxLevelMinter qualified as TxLevelMinter

spend :: Term s PValidator
spend = phoistAcyclic $
  plam $ \_ redeemer ctx -> unTermCont $ do
    return (popaque $ TxLevelMinter.spend # pconstant "BEACON" # redeemer # ctx)

mintLogic :: Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit)
mintLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

mint :: Term s PMintingPolicy
mint = TxLevelMinter.mint mintLogic

validator :: Term s PValidator
validator = Multivalidator.multivalidator mint spend
