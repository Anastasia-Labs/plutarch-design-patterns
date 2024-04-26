{-# LANGUAGE TemplateHaskell #-}

module Plutarch.MultiUTxOIndexerOneToMany (
  withdraw,
  Indices (..),
  WithdrawRedeemer (..),
  PIndices (..),
  PWithdrawRedeemer (..),
) where

import Plutarch.Api.V2 (
  PStakeValidator,
  PStakingCredential (..),
  PTxInInfo,
  PTxOut,
 )
import PlutusTx qualified

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pasInt)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Utils (preverse, (#>))
import PlutusTx (BuiltinData)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data Indices = Indices
  { inIdx :: BuiltinData
  , outIdx :: [BuiltinData]
  }
  deriving stock (Generic, Eq, Show)
PlutusTx.makeIsDataIndexed ''Indices [('Indices, 0)]

newtype WithdrawRedeemer = WithdrawRedeemer {indices :: [Indices]}
  deriving stock (Generic, Eq, Show)

newtype PIndices (s :: S)
  = PIndices (Term s (PDataRecord '["inIdx" ':= PData, "outIdxs" ':= PBuiltinList PData]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PIndices where type DPTStrat _ = PlutusTypeData
deriving anyclass instance
  PTryFrom PData (PAsData PIndices)

instance PUnsafeLiftDecl PIndices where
  type PLifted PIndices = Indices

deriving via
  (DerivePConstantViaData Indices PIndices)
  instance
    PConstantDecl Indices

newtype PWithdrawRedeemer (s :: S)
  = PWithdrawRedeemer (Term s (PDataRecord '["indices" ':= PBuiltinList (PAsData PIndices)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PWithdrawRedeemer where type DPTStrat _ = PlutusTypeData
deriving anyclass instance
  PTryFrom PData (PAsData PWithdrawRedeemer)

data PMyInputAgg (s :: S) = PMyInputAgg (Term s (PBuiltinList PTxInInfo)) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType PMyInputAgg where type DPTStrat _ = PlutusTypeScott

data PMyInOutAgg (s :: S) = PMyInOutAgg (Term s PInteger) (Term s PInteger) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType PMyInOutAgg where type DPTStrat _ = PlutusTypeScott

data PMyOutputAgg (s :: S) = PMyOutputAgg (Term s PInteger) (Term s (PBuiltinList PTxOut)) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType PMyOutputAgg where type DPTStrat _ = PlutusTypeScott

matchInputAgg :: Term s PStakingCredential -> Term s (PMyInputAgg :--> PTxInInfo :--> PMyInputAgg)
matchInputAgg ownValidator = plam $ \p input -> unTermCont $ do
  PMyInputAgg acc count <- pmatchC p
  inputF <- pletFieldsC @'["outRef", "resolved"] input
  resolvedF <- pletFieldsC @'["address"] inputF.resolved
  addressF <- pletFieldsC @'["credential"] resolvedF.address
  PScriptCredential inputCred <- pmatchC addressF.credential
  inputValHash <- pletC $ pfield @"_0" # inputCred
  PStakingHash ownValCred' <- pmatchC ownValidator
  ownValCred <- pletC $ pfield @"_0" # ownValCred'
  PScriptCredential ownValHash' <- pmatchC ownValCred
  ownValHash <- pletC $ pfield @"_0" # ownValHash'
  return $
    pif
      (inputValHash #== ownValHash)
      (pcon (PMyInputAgg (pconcat # (psingleton # input) # acc) (count + 1)))
      p

matchInOutAgg ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PMyInOutAgg :--> PAsData PIndices :--> PMyInOutAgg)
matchInOutAgg inputValidator inputOutputValidator collectiveOutputValidator outputs scriptInputs = plam $ \p indicesData -> unTermCont $ do
  PMyInOutAgg prevInIx latestOutIx inputCountSoFar <- pmatchC p
  indices <- pletC $ punsafeCoerce @_ @_ @PIndices (pfromData indicesData)
  indicesF <- pletFieldsC @'["inIdx", "outIdxs"] indices
  curInIx <- pletC $ pasInt # indicesF.inIdx
  outIxs <- pletC $ pmap # pasInt # indicesF.outIdxs
  input <- pletC $ pelemAt # curInIx # scriptInputs
  inInputF <- pletFieldsC @'["outRef", "resolved"] input
  return $
    pif
      (inputValidator # input #&& curInIx #> prevInIx)
      ( unTermCont $ do
          outputAggregated <-
            pletC $
              pfoldl
                # matchOutputAgg inputOutputValidator outputs inInputF.resolved
                # pcon (PMyOutputAgg latestOutIx pnil 0)
                # outIxs
          PMyOutputAgg newLatestOutIx outUTxOsReversed outputCOunt <- pmatchC outputAggregated
          return $
            pif
              (collectiveOutputValidator # (preverse # outUTxOsReversed) # outputCOunt)
              (pcon (PMyInOutAgg curInIx newLatestOutIx inputCountSoFar))
              perror
      )
      perror

matchOutputAgg ::
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PTxOut ->
  Term s (PMyOutputAgg :--> PInteger :--> PMyOutputAgg)
matchOutputAgg inputOutputValidator outputs input = plam $ \p currOutIx -> unTermCont $ do
  PMyOutputAgg prevOutIx utxosSoFar count <- pmatchC p
  outUTxO <- pletC $ pelemAt # currOutIx # outputs
  return $
    pif
      (currOutIx #> prevOutIx #&& inputOutputValidator # input # outUTxO)
      (pcon (PMyOutputAgg currOutIx (pconcat # utxosSoFar # (psingleton # outUTxO)) (count + 1)))
      perror

withdrawLogic ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic inputValidator inputOutputValidator collectiveOutputValidator =
  plam $ \redData ownValidator txInfo -> unTermCont $ do
    red <- pletC $ punsafeCoerce @_ @_ @PWithdrawRedeemer redData
    redF <- pletFieldsC @'["indices"] red
    txInfoF <- pletFieldsC @'["inputs", "outputs"] txInfo
    inputAggregated <-
      pletC $
        pfoldl
          # matchInputAgg ownValidator
          # pcon (PMyInputAgg pnil 0)
          # txInfoF.inputs
    PMyInputAgg scriptInputs scriptInputCount <- pmatchC inputAggregated
    inoutAggregated <-
      pletC $
        pfoldl
          # matchInOutAgg inputValidator inputOutputValidator collectiveOutputValidator txInfoF.outputs scriptInputs
          # pcon (PMyInOutAgg (-1) (-1) 0)
          # redF.indices
    PMyInOutAgg _ _ inputIndexCount <- pmatchC inoutAggregated
    pure $
      pif
        (scriptInputCount #== inputIndexCount)
        (pconstant ())
        perror

withdraw ::
  Term s (PTxInInfo :--> PBool) ->
  Term s (PTxOut :--> PTxOut :--> PBool) ->
  Term s (PBuiltinList PTxOut :--> PInteger :--> PBool) ->
  Term s PStakeValidator
withdraw inputValidator inputOutputValidator collectiveOutputValidator =
  StakeValidator.withdraw
    ( withdrawLogic
        inputValidator
        inputOutputValidator
        collectiveOutputValidator
    )
