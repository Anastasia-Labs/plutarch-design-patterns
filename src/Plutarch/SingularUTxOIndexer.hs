{-# LANGUAGE TemplateHaskell #-}

module Plutarch.SingularUTxOIndexer (
  spend,
  SpendRedeemer (..),
  PSpendRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PRedeemer (..),
  PScriptContext (..),
  PScriptInfo (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusTx qualified

data SpendRedeemer = SpendRedeemer
  { inIdx :: PlutusTx.BuiltinData
  , outIdx :: PlutusTx.BuiltinData
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''SpendRedeemer [('SpendRedeemer, 0)]

data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pspendRedeemer'inIdx :: Term s (PAsData PData)
  , pspendRedeemer'outIdx :: Term s (PAsData PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PSpendRedeemer

deriving via
  DeriveDataPLiftable PSpendRedeemer SpendRedeemer
  instance
    PLiftable PSpendRedeemer

spend ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PScriptContext :--> POpaque)
spend f =
  plam $ \ctx -> P.do
    PScriptContext
      { pscriptContext'txInfo
      , pscriptContext'redeemer
      , pscriptContext'scriptInfo
      } <-
      pmatch ctx
    PRedeemer redeemer <- pmatch pscriptContext'redeemer
    PSpendRedeemer {pspendRedeemer'inIdx, pspendRedeemer'outIdx} <-
      pmatch $ pfromData $ pparseData @PSpendRedeemer redeemer
    PSpendingScript ownRef _ <- pmatch pscriptContext'scriptInfo
    PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch pscriptContext'txInfo
    input <-
      plet $
        pfromData $
          pelemAt
            # (pasInt # pfromData pspendRedeemer'inIdx)
            # pfromData ptxInfo'inputs
    PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch input
    pif
      (ptraceInfoIfFalse "Indicated input must match the spending one" (ownRef #== ptxInInfo'outRef))
      ( popaque $
          f
            # ptxInInfo'resolved
            # pfromData
              ( pelemAt
                  # (pasInt # pfromData pspendRedeemer'outIdx)
                  # pfromData ptxInfo'outputs
              )
      )
      perror
