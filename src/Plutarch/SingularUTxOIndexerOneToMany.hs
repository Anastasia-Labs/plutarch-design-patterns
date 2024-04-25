{-# LANGUAGE OverloadedRecordDot #-}

module Plutarch.SingularUTxOIndexerOneToMany (
  spend,
  SpendRedeemer (..),
  PSpendRedeemer (..),
) where

import Plutarch.Api.V2 (
  PScriptPurpose (..),
  PTxInInfo,
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

spend ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PList PTxOut :--> PInteger :--> PBool) ->
  Term s PValidator
spend inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \_datum redeemer ctx -> unTermCont $ do
    red <- pletC $ punsafeCoerce @_ @_ @PSpendRedeemer redeemer
    redF <- pletFieldsC @'["inIx", "outIxs"] red
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC ctxF.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'
    txInfoF <- pletFieldsC @'["inputs", "outputs"] ctxF.txInfo
    input <- pletC $ pelemAt @PBuiltinList # (pasInt # redF.inIx) # txInfoF.inputs
    outIxs <- pletC $ pmap # pasInt # redF.outIxs
    inInputF <- pletFieldsC @'["outRef", "resolved"] input
    aggregated <-
      pletC $
        pfoldr
          # (matchAgg inputOutputValidator # inInputF.resolved # txInfoF.outputs)
          # pcon (PMyAggregator (plength # pfromData txInfoF.outputs) pnil 0)
          # outIxs

    return $ pmatch aggregated $ \case
      PMyAggregator _ outTxOuts outputCount -> unTermCont $ do
        return $
          popaque $
            pif
              ( ptraceIfFalse "Indicated input must match the spending one" (ownRef #== inInputF.outRef)
                  #&& ptraceIfFalse "Input Validator Fails" (inputValidator # input)
              )
              (collectiveOutputValidator # outTxOuts # outputCount)
              perror

matchAgg :: Term s (PTxOut :--> PTxOut :--> PBool) -> Term s (PTxOut :--> PBuiltinList PTxOut :--> PInteger :--> PMyAggregator :--> PMyAggregator)
matchAgg inputOutputValidator = plam $ \input outputs curIdx p -> unTermCont $ do
  PMyAggregator prevIdx acc count <- pmatchC p
  return $
    pif
      (curIdx #== prevIdx)
      ( P.do
          let outOutput = pelemAt @PBuiltinList # curIdx # outputs
          pif
            (ptraceIfFalse "Input Output Validator Fails" (inputOutputValidator # input # outOutput))
            (pcon (PMyAggregator curIdx (pconcat # acc #$ psingleton # (pelemAt @PBuiltinList # curIdx # outputs)) (count + 1)))
            perror
      )
      perror
