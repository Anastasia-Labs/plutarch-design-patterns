{-# LANGUAGE TemplateHaskell #-}

module Plutarch.SingularUTxOIndexer (
  spend,
  SpendRedeemer (..),
  PSpendRedeemer (..),
) where

import Plutarch.Api.V2 (
  PScriptPurpose (..),
  PTxOut,
  PValidator,
 )
import Plutarch.Builtin (pasInt)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data SpendRedeemer = SpendRedeemer
  { inIdx :: PlutusTx.BuiltinData
  , outIdx :: PlutusTx.BuiltinData
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeLift ''SpendRedeemer
PlutusTx.makeIsDataIndexed ''SpendRedeemer [('SpendRedeemer, 0)]

newtype PSpendRedeemer (s :: S)
  = PSpendRedeemer (Term s (PDataRecord '["inIdx" ':= PData, "outIdx" ':= PData]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PSpendRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PSpendRedeemer

spend :: Term s (PTxOut :--> PTxOut :--> PBool) -> Term s PValidator
spend f =
  plam $ \_datum redeemer ctx -> unTermCont $ do
    let red = punsafeCoerce @_ @_ @PSpendRedeemer redeemer
    redF <- pletFieldsC @'["inIdx", "outIdx"] red
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC ctxF.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'
    txInfoF <- pletFieldsC @'["inputs", "outputs"] ctxF.txInfo
    let inIdx = pasInt # redF.inIdx
        outIdx = pasInt # redF.outIdx
        outOutput = pelemAt @PBuiltinList # outIdx # txInfoF.outputs
    inInputF <- pletFieldsC @'["outRef", "resolved"] (pelemAt @PBuiltinList # inIdx # txInfoF.inputs)
    return $
      popaque $
        pif
          (ptraceIfFalse "Indicated input must match the spending one" (ownRef #== inInputF.outRef))
          (f # inInputF.resolved # outOutput)
          perror
