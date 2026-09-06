{-# LANGUAGE TemplateHaskell #-}

module Plutarch.SingularUTxOIndexerOneToMany (
  spend,
  SpendRedeemer (..),
  PSpendRedeemer (..),
  matchAgg,
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
  { inIx :: PlutusTx.BuiltinData
  , outIxs :: [PlutusTx.BuiltinData]
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''SpendRedeemer [('SpendRedeemer, 0)]

data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pspendRedeemer'inIx :: Term s (PAsData PData)
  , pspendRedeemer'outIxs ::
      Term s (PAsData (PBuiltinList (PAsData PData)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PSpendRedeemer

deriving via
  DeriveDataPLiftable PSpendRedeemer SpendRedeemer
  instance
    PLiftable PSpendRedeemer

data PMyAggregator (s :: S)
  = PMyAggregator
      (Term s PInteger)
      (Term s (PBuiltinList PTxOut))
      (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)
  deriving (PlutusType) via DeriveAsSOPStruct PMyAggregator

spend ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PScriptContext :--> PUnit)
spend inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \ctx -> P.do
    PScriptContext
      { pscriptContext'txInfo
      , pscriptContext'redeemer
      , pscriptContext'scriptInfo
      } <-
      pmatch ctx
    PRedeemer redeemer <- pmatch pscriptContext'redeemer
    PSpendRedeemer {pspendRedeemer'inIx, pspendRedeemer'outIxs} <-
      pmatch $ pfromData $ pparseData @PSpendRedeemer redeemer
    PSpendingScript ownRef _ <- pmatch pscriptContext'scriptInfo
    PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch pscriptContext'txInfo
    input <-
      plet $
        pfromData $
          pelemAt
            # (pasInt # pfromData pspendRedeemer'inIx)
            # pfromData ptxInfo'inputs
    outIxs <-
      plet $
        pmap
          # plam (\indexData -> pasInt # pfromData indexData)
          # pfromData pspendRedeemer'outIxs
    PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch input
    aggregated <-
      plet $
        pfoldr
          # (matchAgg inputOutputValidator # ptxInInfo'resolved # pfromData ptxInfo'outputs)
          # pcon (PMyAggregator (plength # pfromData ptxInfo'outputs) pnil 0)
          # outIxs
    PMyAggregator _ outTxOuts outputCount <- pmatch aggregated
    pif
      ( ptraceInfoIfFalse "Indicated input must match the spending one" (ownRef #== ptxInInfo'outRef)
          #&& ptraceInfoIfFalse "Input Validator Fails" (inputValidator # input)
          #&& ptraceInfoIfFalse "Collective Output Validator Fails" (collectiveOutputValidator # outTxOuts # outputCount)
      )
      (pconstant ())
      perror

matchAgg ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term
    s
    ( PTxOut
        :--> PBuiltinList (PAsData PTxOut)
        :--> PInteger
        :--> PMyAggregator
        :--> PMyAggregator
    )
matchAgg inputOutputValidator =
  plam $ \input outputs curIdx aggregate -> P.do
    PMyAggregator prevIdx acc count <- pmatch aggregate
    pif
      (ptraceInfoIfFalse (pshow prevIdx) (curIdx #< prevIdx))
      ( P.do
          let outOutput = pfromData $ pelemAt # curIdx # outputs
          pif
            (ptraceInfoIfFalse "Input Output Validator Fails" (inputOutputValidator # input # outOutput))
            ( pcon $
                PMyAggregator
                  curIdx
                  (pconcat # acc # (psingleton # outOutput))
                  (count + 1)
            )
            perror
      )
      perror
