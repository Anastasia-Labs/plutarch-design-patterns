module Spec.Utils (
  inputOutputValidator,
  inputValidator,
  collectiveOutputValidator,
  evalSucceeds,
  genByteString,
  mkAddressFromByteString,
  mkScriptCredential,
  mkTxInInfo,
  mkTxOut,
  mkScriptContext,
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (Config (NoTracing))
import Plutarch.LedgerApi.V3 (PTxInInfo, PTxOut)
import Plutarch.Prelude
import Plutarch.Test.Unit (TermResult (..), evalTermResult)
import PlutusLedgerApi.V3 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo,
  ScriptPurpose,
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  Value (..),
  always,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Test.Tasty.QuickCheck (Gen, Property, counterexample, elements, property, vectorOf)

inputValidator :: Term s (PTxInInfo :--> PBool)
inputValidator = phoistAcyclic $
  plam $
    \_ -> pcon PTrue

inputOutputValidator :: Term s (PTxOut :--> PTxOut :--> PBool)
inputOutputValidator = phoistAcyclic $
  plam $
    \_ _ -> pcon PTrue

collectiveOutputValidator :: Term s (PBuiltinList PTxOut :--> PInteger :--> PBool)
collectiveOutputValidator = phoistAcyclic $
  plam $
    \_ _ -> pcon PTrue

evalSucceeds ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Property
evalSucceeds term =
  case evalTermResult NoTracing term of
    FailedToCompile err -> counterexample ("Failed to compile: " <> show err) False
    FailedToEvaluate err _ -> counterexample ("Failed to evaluate: " <> show err) False
    Evaluated _ _ -> property True

genByteString :: Int -> Gen BuiltinByteString
genByteString n = do
  member <- vectorOf (n * 2) $ elements (['a' .. 'f'] ++ ['0' .. '9'])
  pure $ stringToBuiltinByteString member

mkAddressFromByteString :: BuiltinByteString -> Address
mkAddressFromByteString = flip Address Nothing . mkScriptCredential

mkScriptCredential :: BuiltinByteString -> Credential
mkScriptCredential = ScriptCredential . ScriptHash

mkTxInInfo :: TxOutRef -> Address -> Value -> TxInInfo
mkTxInInfo outRef address value =
  TxInInfo outRef (mkTxOut address value)

mkTxOut :: Address -> Value -> TxOut
mkTxOut address value = TxOut address value NoOutputDatum Nothing

mkScriptContext ::
  [TxInInfo] ->
  [TxOut] ->
  Value ->
  [(Credential, Integer)] ->
  [(ScriptPurpose, Redeemer)] ->
  Redeemer ->
  ScriptInfo ->
  ScriptContext
mkScriptContext inputs outputs mint withdrawals redeemers redeemer scriptInfo =
  ScriptContext txInfo redeemer scriptInfo
  where
    txInfo =
      TxInfo
        { txInfoInputs = inputs
        , txInfoReferenceInputs = []
        , txInfoOutputs = outputs
        , txInfoFee = 0
        , txInfoMint = UnsafeMintValue $ getValue mint
        , txInfoTxCerts = []
        , txInfoWdrl = Map.unsafeFromList $ fmap (\(credential, amount) -> (credential, fromInteger amount)) withdrawals
        , txInfoValidRange = always
        , txInfoSignatories = []
        , txInfoRedeemers = Map.unsafeFromList redeemers
        , txInfoData = Map.empty
        , txInfoId = TxId ""
        , txInfoVotes = Map.empty
        , txInfoProposalProcedures = []
        , txInfoCurrentTreasuryAmount = Nothing
        , txInfoTreasuryDonation = Nothing
        }
