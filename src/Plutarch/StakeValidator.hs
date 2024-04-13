module Plutarch.StakeValidator (
  spend,
  withdraw,
) where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PScriptPurpose (..), PStakeValidator, PStakingCredential (..), PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Prelude
import Plutarch.Utils (ptryOwnInput)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

spend :: ClosedTerm PValidator
spend = plam $ \_ _ ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PSpending ownRef' <- pmatchC ctxF.purpose
  ownRef <- pletC $ pfield @"_0" # ownRef'
  txInfoF <- pletFieldsC @'["inputs", "wdrl"] ctxF.txInfo
  ownInput <- pletC $ ptryOwnInput # txInfoF.inputs # ownRef
  ownInputF <- pletFieldsC @'["address"] ownInput
  let ownScriptCred = (pcon . PStakingHash) $ pdcons @"_0" # (pfield @"credential" # ownInputF.address) # pdnil
  return $
    pmatch (AssocMap.plookup # ownScriptCred # txInfoF.wdrl) $ \case
      PJust _ -> (popaque $ pconstant ())
      PNothing -> perror

withdraw :: Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit) -> Term s PStakeValidator
withdraw withdrawLogic =
  plam $ \redeemer ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PRewarding stakeCred' <- pmatchC ctxF.purpose
    stakeCred <- pletC $ pfield @"_0" # stakeCred'
    return $
      popaque $
        withdrawLogic # redeemer # stakeCred # ctxF.txInfo
