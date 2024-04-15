{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.MerkelizedValidator (
  spend,
  withdraw,
) where

import Plutarch.Api.V1 qualified as V1
import Plutarch.Api.V2 (PScriptPurpose (..), PStakeValidator, PStakingCredential (..))
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.Record (mkRecordConstr, (.=))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx (BuiltinData)
import "liqwid-plutarch-extra" Plutarch.Extra.Map (ptryLookup)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data WithdrawRedeemer = WithdrawRedeemer
  { inputState :: [BuiltinData]
  , outputState :: [BuiltinData]
  }
  deriving stock (Generic, Eq, Show)

newtype PWithdrawRedeemer (s :: S)
  = PWithdrawRedeemer (Term s (PDataRecord '["inputState" ':= PBuiltinList PData, "outputState" ':= PBuiltinList PData]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PWithdrawRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PWithdrawRedeemer

spend :: Term s PStakingCredential -> Term s (PBuiltinList PData) -> Term s (V1.PMap 'V1.Unsorted V1.PScriptPurpose V1.PRedeemer) -> Term s (PBuiltinList PData)
spend stakCred inputState redeemers = unTermCont $ do
  spending <- pletC $ mkRecordConstr PRewarding $ #_0 .= pdata stakCred
  redeemer' <- pletC $ ptryLookup # spending # redeemers
  V1.PRedeemer redeemer <- pmatchC redeemer'
  let red = punsafeCoerce @_ @_ @PWithdrawRedeemer redeemer
  redF <- pletFieldsC @'["inputState", "outputState"] red
  return $
    pif
      (inputState #== redF.inputState)
      (redF.outputState)
      perror

withdraw :: Term s (PBuiltinList PData :--> PBuiltinList PData) -> Term s PStakeValidator
withdraw f =
  plam $ \redeemer ctx -> unTermCont $ do
    let red = punsafeCoerce @_ @_ @PWithdrawRedeemer redeemer
    redF <- pletFieldsC @'["inputState", "outputState"] red
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PRewarding _ <- pmatchC ctxF.purpose
    return $
      popaque $
        (f # redF.inputState) #== redF.outputState
