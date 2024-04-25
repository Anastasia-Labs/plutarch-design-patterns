{-# LANGUAGE OverloadedRecordDot #-}

module Plutarch.SingularUTxOIndexerOneToMany (
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
import PlutusTx (BuiltinData)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data SpendRedeemer = SpendRedeemer
  { inIx :: BuiltinData
  , outIxs :: [BuiltinData]
  }
  deriving stock (Generic, Eq, Show)

newtype PSpendRedeemer (s :: S)
  = PSpendRedeemer (Term s (PDataRecord '["inIx" ':= PData, "outIxs" ':= PBuiltinList PData]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PSpendRedeemer where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PSpendRedeemer

data PMyAggregator (s :: S) = PMyAggregator (Term s PInteger) (Term s (PList PTxOut)) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType PMyAggregator where type DPTStrat _ = PlutusTypeScott

spend :: Term s (PTxOut :--> PBool)
  -> Term s (PTxOut :--> PTxOut :--> PBool)
  -> Term s (PBuiltinList PTxOut :--> PInteger :--> PBool)
  -> Term s PValidator
spend inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \_datum redeemer ctx -> unTermCont $ do
    let red = punsafeCoerce @_ @_ @PSpendRedeemer redeemer
    redF <- pletFieldsC @'["inIx", "outIxs"] red
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC ctxF.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'
    txInfoF <- pletFieldsC @'["inputs", "outputs"] ctxF.txInfo
    let inIx = pasInt # redF.inIx
        outIxs = pmap # pasInt # redF.outIxs
        aggregated =
            pfoldr # 
              (plam $ \curIdx p ->
                pmatch p $ \case
                    PMyAggregator prevIdx acc count ->
                      pif
                        (curIdx #< prevIdx)
                        (pcon (PMyAggregator curIdx (pconcat # acc #$ psingleton # (pelemAt @PBuiltinList # curIdx # txInfoF.outputs)) (count + 1)))
                        perror
              )
              # pcon (PMyAggregator (plength # txInfoF.outputs) pnil 0) # outIxs

    return $ pmatch aggregated $ \case
        PMyAggregator _ outTxOuts outputCount -> unTermCont $ do
          let outOutput = pelemAt @PBuiltinList # outIxs # txInfoF.outputs
          inInputF <- pletFieldsC @'["outRef", "resolved"] (pelemAt @PBuiltinList # inIx # txInfoF.inputs)
          return $
            popaque $
              pif
                (ptraceIfFalse "Indicated input must match the spending one" (ownRef #== inInputF.outRef))
                (inputOutputValidator # inInputF.resolved # outOutput)
                perror
