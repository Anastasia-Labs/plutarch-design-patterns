{-# LANGUAGE TemplateHaskell #-}

module Plutarch.MultiUTxOIndexer (
  withdraw,
  Indices (..),
  WithdrawRedeemer (..),
  PIndices (..),
  PWithdrawRedeemer (..),
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
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import PlutusTx (BuiltinData)
import PlutusTx qualified

data Indices = Indices
  { inIdx :: BuiltinData
  , outIdx :: BuiltinData
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
  , pindices'outIdx :: Term s (PAsData PData)
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

matchInputAgg ::
  Term s PCredential ->
  Term s (PAsData PTxInInfo :--> PMyInputAgg :--> PMyInputAgg)
matchInputAgg ownValidator =
  plam $ \inputData aggregate -> P.do
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
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PMyInOutAgg :--> PAsData PIndices :--> PMyInOutAgg)
matchInOutAgg outputs inoutValidator scriptInputs =
  plam $ \aggregate indicesData -> P.do
    PMyInOutAgg in0 out0 count <- pmatch aggregate
    PIndices {pindices'inIdx, pindices'outIdx} <- pmatch $ pfromData indicesData
    in1 <- plet $ pasInt # pfromData pindices'inIdx
    out1 <- plet $ pasInt # pfromData pindices'outIdx
    pif
      (in1 #> in0 #&& out1 #> out0)
      ( P.do
          let input = pelemAt # in1 # scriptInputs
              output = pfromData $ pelemAt # out1 # outputs
          PTxInInfo {ptxInInfo'resolved} <- pmatch input
          pif
            (ptraceInfoIfFalse "Input Output Validator Fails" (inoutValidator # ptxInInfo'resolved # output))
            (pcon $ PMyInOutAgg in1 out1 (count + 1))
            perror
      )
      perror

withdrawLogic ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PData :--> PCredential :--> PTxInfo :--> PUnit)
withdrawLogic inoutValidator =
  plam $ \redData ownValidator txInfo -> P.do
    PWithdrawRedeemer redIndices <-
      pmatch $ pfromData $ pparseData @PWithdrawRedeemer redData
    PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
    inputAggregated <-
      plet $
        pfoldr
          # matchInputAgg ownValidator
          # pcon (PMyInputAgg pnil 0)
          # pfromData ptxInfo'inputs
    PMyInputAgg scriptInputs scriptInputCount <- pmatch inputAggregated
    inoutAggregated <-
      plet $
        pfoldl
          # matchInOutAgg (pfromData ptxInfo'outputs) inoutValidator scriptInputs
          # pcon (PMyInOutAgg (-1) (-1) 0)
          # pfromData redIndices
    PMyInOutAgg _ _ inputIndexCount <- pmatch inoutAggregated
    pif
      (scriptInputCount #== inputIndexCount)
      (pconstant ())
      perror

withdraw ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PScriptContext :--> PUnit)
withdraw inoutValidator = StakeValidator.withdraw (withdrawLogic inoutValidator)
