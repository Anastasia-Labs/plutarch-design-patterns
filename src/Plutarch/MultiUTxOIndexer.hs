{-# LANGUAGE TemplateHaskell #-}

module Plutarch.MultiUTxOIndexer (
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
import Plutarch.Utils ((#>))
import PlutusTx (BuiltinData)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

data Indices = Indices
  { inIdx :: BuiltinData
  , outIdx :: BuiltinData
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeLift ''Indices
PlutusTx.makeIsDataIndexed ''Indices [('Indices, 0)]

data WithdrawRedeemer
  = None
  | WithdrawRedeemer {indices :: [Indices]}
  deriving stock (Generic, Eq, Show)

PlutusTx.makeLift ''WithdrawRedeemer
PlutusTx.makeIsDataIndexed ''WithdrawRedeemer [('None, 0), ('WithdrawRedeemer, 1)]

newtype PIndices (s :: S)
  = PIndices (Term s (PDataRecord '["inIdx" ':= PData, "outIdx" ':= PData]))
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

data PWithdrawRedeemer (s :: S)
  = PNone (Term s (PDataRecord '[]))
  | PWithdrawRedeemer (Term s (PDataRecord '["indices" ':= PBuiltinList (PAsData PIndices)]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PWithdrawRedeemer where type DPTStrat _ = PlutusTypeData
deriving anyclass instance
  PTryFrom PData (PAsData PWithdrawRedeemer)
instance PUnsafeLiftDecl PWithdrawRedeemer where
  type PLifted PWithdrawRedeemer = WithdrawRedeemer

deriving via
  (DerivePConstantViaData WithdrawRedeemer PWithdrawRedeemer)
  instance
    PConstantDecl WithdrawRedeemer

data PMyInputAgg (s :: S) = PMyInputAgg (Term s (PBuiltinList PTxInInfo)) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType PMyInputAgg where type DPTStrat _ = PlutusTypeScott

data PMyInOutAgg (s :: S) = PMyInOutAgg (Term s PInteger) (Term s PInteger) (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType PMyInOutAgg where type DPTStrat _ = PlutusTypeScott

matchInputAgg :: Term s PStakingCredential -> Term s (PTxInInfo :--> PMyInputAgg :--> PMyInputAgg)
matchInputAgg ownValidator = plam $ \input p -> unTermCont $ do
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

matchInOutAgg :: Term s (PBuiltinList PTxOut) -> Term s (PTxOut :--> PTxOut :--> PBool) -> Term s (PBuiltinList PTxInInfo) -> Term s (PMyInOutAgg :--> PAsData PIndices :--> PMyInOutAgg)
matchInOutAgg outputs inoutValidator scriptInputs = plam $ \p indicesData -> unTermCont $ do
  PMyInOutAgg in0 out0 count <- pmatchC p
  indices <- pletC $ punsafeCoerce @_ @_ @PIndices (pfromData indicesData)
  indicesF <- pletFieldsC @'["inIdx", "outIdx"] indices
  in1 <- pletC $ pasInt # indicesF.inIdx
  out1 <- pletC $ pasInt # indicesF.outIdx
  return $
    pif
      (in1 #> in0 #&& out1 #> out0)
      ( do
          let input = pelemAt # in1 # scriptInputs
              output = pelemAt # out1 # outputs
              inTxOut = pfield @"resolved" # input
          pif
            (ptraceIfFalse "Input Output Validator Fails" (inoutValidator # inTxOut # output))
            (pcon $ PMyInOutAgg in1 out1 (count + 1))
            perror
      )
      perror

withdrawLogic :: Term s (PTxOut :--> PTxOut :--> PBool) -> Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic inoutValidator =
  plam $ \redData ownValidator txInfo -> unTermCont $ do
    red <- pletC $ punsafeCoerce @_ @_ @PWithdrawRedeemer redData
    PWithdrawRedeemer wrdm <- pmatchC red
    redF <- pletFieldsC @'["indices"] wrdm
    txInfoF <- pletFieldsC @'["inputs", "outputs"] txInfo
    inputAggregated <-
      pletC $
        pfoldr
          # matchInputAgg ownValidator
          # pcon (PMyInputAgg pnil 0)
          # txInfoF.inputs
    PMyInputAgg scriptInputs scriptInputCount <- pmatchC inputAggregated
    inoutAggregated <-
      pletC $
        pfoldl
          # matchInOutAgg txInfoF.outputs inoutValidator scriptInputs
          # pcon (PMyInOutAgg (-1) (-1) 0)
          # redF.indices
    PMyInOutAgg _ _ inputIndexCount <- pmatchC inoutAggregated

    pure $
      pif
        (scriptInputCount #== inputIndexCount)
        (pconstant ())
        perror

withdraw :: Term s (PTxOut :--> PTxOut :--> PBool) -> Term s PStakeValidator
withdraw inoutValidator = StakeValidator.withdraw (withdrawLogic inoutValidator)
