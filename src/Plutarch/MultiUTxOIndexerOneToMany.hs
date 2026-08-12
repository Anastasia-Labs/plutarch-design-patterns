{-# LANGUAGE TemplateHaskell #-}

module Plutarch.MultiUTxOIndexerOneToMany (
  withdraw,
  Indices (..),
  WithdrawRedeemer (..),
  PIndices (..),
  PWithdrawRedeemer (..),
  matchInputAgg,
  matchInOutAgg,
  matchOutputAgg,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PScriptContext,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude hiding ((#>))
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Utils (preverse, (#>))
import PlutusTx (BuiltinData)
import PlutusTx qualified

data Indices = Indices
  { inIdx :: BuiltinData
  , outIdx :: [BuiltinData]
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''Indices [('Indices, 0)]

data WithdrawRedeemer
  = None
  | WithdrawRedeemer {indices :: [Indices]}
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''WithdrawRedeemer [('None, 0), ('WithdrawRedeemer, 1)]

data PIndices (s :: S) = PIndices
  { pindices'inIdx :: Term s (PAsData PData)
  , pindices'outIdxs ::
      Term s (PAsData (PBuiltinList (PAsData PData)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PIndices

deriving via
  DeriveDataPLiftable PIndices Indices
  instance
    PLiftable PIndices

data PWithdrawRedeemer (s :: S)
  = PNone
  | PWithdrawRedeemer
      (Term s (PAsData (PBuiltinList (PAsData PIndices))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PWithdrawRedeemer

deriving via
  DeriveDataPLiftable PWithdrawRedeemer WithdrawRedeemer
  instance
    PLiftable PWithdrawRedeemer

data PMyInputAgg (s :: S)
  = PMyInputAgg (Term s (PBuiltinList PTxInInfo)) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)
  deriving (PlutusType) via DeriveAsSOPStruct PMyInputAgg

data PMyInOutAgg (s :: S)
  = PMyInOutAgg (Term s PInteger) (Term s PInteger) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)
  deriving (PlutusType) via DeriveAsSOPStruct PMyInOutAgg

data PMyOutputAgg (s :: S)
  = PMyOutputAgg
      (Term s PInteger)
      (Term s (PBuiltinList PTxOut))
      (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PEq, PShow)
  deriving (PlutusType) via DeriveAsSOPStruct PMyOutputAgg

matchInputAgg ::
  Term s PCredential ->
  Term s (PMyInputAgg :--> PAsData PTxInInfo :--> PMyInputAgg)
matchInputAgg ownValidator =
  plam $ \aggregate inputData -> P.do
    PMyInputAgg acc count <- pmatch aggregate
    input <- plet $ pfromData inputData
    PTxInInfo {ptxInInfo'resolved} <- pmatch input
    PTxOut {ptxOut'address} <- pmatch ptxInInfo'resolved
    PAddress {paddress'credential} <- pmatch ptxOut'address
    PScriptCredential inputCred <- pmatch paddress'credential
    PScriptCredential ownValHash <- pmatch ownValidator
    pif
      (inputCred #== ownValHash)
      (pcon $ PMyInputAgg (pcons # input # acc) (count + 1))
      aggregate

matchInOutAgg ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PMyInOutAgg :--> PAsData PIndices :--> PMyInOutAgg)
matchInOutAgg inputValidator inputOutputValidator collectiveOutputValidator outputs scriptInputs =
  plam $ \aggregate indicesData -> P.do
    PMyInOutAgg prevInIx latestOutIx inputCountSoFar <- pmatch aggregate
    PIndices {pindices'inIdx, pindices'outIdxs} <- pmatch $ pfromData indicesData
    curInIx <- plet $ pasInt # pfromData pindices'inIdx
    outIxs <-
      plet $
        pmap
          # plam (\indexData -> pasInt # pfromData indexData)
          # pfromData pindices'outIdxs
    input <- plet $ pelemAt # curInIx # scriptInputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch input
    pif
      (inputValidator # input #&& curInIx #> prevInIx)
      ( P.do
          outputAggregated <-
            plet $
              pfoldl
                # matchOutputAgg inputOutputValidator outputs ptxInInfo'resolved
                # pcon (PMyOutputAgg latestOutIx pnil 0)
                # outIxs
          PMyOutputAgg newLatestOutIx outUTxOsReversed outputCount <-
            pmatch outputAggregated
          pif
            (collectiveOutputValidator # (preverse # outUTxOsReversed) # outputCount)
            (pcon $ PMyInOutAgg curInIx newLatestOutIx (inputCountSoFar + 1))
            perror
      )
      perror

matchOutputAgg ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PTxOut ->
  Term s (PMyOutputAgg :--> PInteger :--> PMyOutputAgg)
matchOutputAgg inputOutputValidator outputs input =
  plam $ \aggregate currOutIx -> P.do
    PMyOutputAgg prevOutIx utxosSoFar count <- pmatch aggregate
    outUTxO <- plet $ pfromData $ pelemAt # currOutIx # outputs
    pif
      (currOutIx #> prevOutIx #&& inputOutputValidator # input # outUTxO)
      ( pcon $
          PMyOutputAgg
            currOutIx
            (pconcat # utxosSoFar # (psingleton # outUTxO))
            (count + 1)
      )
      perror

withdrawLogic ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PData :--> PCredential :--> PTxInfo :--> PUnit)
withdrawLogic inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \redData ownValidator txInfo -> P.do
    PWithdrawRedeemer redIndices <-
      pmatch $ pfromData $ pparseData @PWithdrawRedeemer redData
    PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
    inputAggregated <-
      plet $
        pfoldl
          # matchInputAgg ownValidator
          # pcon (PMyInputAgg pnil 0)
          # pfromData ptxInfo'inputs
    PMyInputAgg scriptInputs scriptInputCount <- pmatch inputAggregated
    inoutAggregated <-
      plet $
        pfoldl
          # matchInOutAgg
            inputValidator
            inputOutputValidator
            collectiveOutputValidator
            (pfromData ptxInfo'outputs)
            scriptInputs
          # pcon (PMyInOutAgg (-1) (-1) 0)
          # pfromData redIndices
    PMyInOutAgg _ _ inputIndexCount <- pmatch inoutAggregated
    pif
      (scriptInputCount #== inputIndexCount)
      (pconstant ())
      perror

withdraw ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PScriptContext :--> PUnit)
withdraw inputValidator inputOutputValidator collectiveOutputValidator =
  StakeValidator.withdraw
    ( withdrawLogic
        inputValidator
        inputOutputValidator
        collectiveOutputValidator
    )
