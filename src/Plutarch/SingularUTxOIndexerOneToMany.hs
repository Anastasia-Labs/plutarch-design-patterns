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

spend :: Term s (PTxOut :--> PBool) 
  -> Term s (PTxOut :--> PTxOut :--> PBool)
  -> Term s (PBuiltinList PTxOut :--> PInteger :--> PBool)
  -> Term s PValidator
spend inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \_datum redeemer ctx -> unTermCont $ do
    let red = punsafeCoerce @_ @_ @PSpendRedeemer redeemer
    redF <- pletFieldsC @'["indx", "outIxs"] red
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC ctxF.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'
    txInfoF <- pletFieldsC @'["inputs", "outputs"] ctxF.txInfo
    let inIx = pasInt # redF.inIx
        outIxs = pmap # pasInt # redF.outIxs
        (_, outTxOuts, outputCount) = 
            pfoldr # 
              plan (\(curIdx (prevIdx, acc, count) -> pif 
                  (curIdx #< prevIdx)
                  (curIdx, pconcat # acc # psingleton (pelemAt @PBuiltinList # curIdx # txInfoF.outputs), count + 1))
                  perror
              ) #
              (P.length # outputs, [], 0) #
              outIxs

        outOutput = pelemAt @PBuiltinList # outIdx # txInfoF.outputs
    inInputF <- pletFieldsC @'["outRef", "resolved"] (pelemAt @PBuiltinList # inIx # txInfoF.inputs)
    return $
      popaque $
        pif
          (ptraceIfFalse "Indicated input must match the spending one" (ownRef #== inInputF.outRef))
          (inputOutputValidator # inInputF.resolved # outOutput)
          perror
