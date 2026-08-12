{- |
Module      : Spec.MultiUTxOIndexerSpec
Description : Test suite for MultiUTxO Indexer validation in a Plutarch-based smart contract setting.
-}
module Spec.MultiUTxOIndexerSpec (
  propertyTest,
  unitTest,
  validator,
) where

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.MultiUTxOIndexer qualified as MultiUTxOIndexer
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Unit (testEval, testEvalFail)
import PlutusLedgerApi.V3 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  Redeemer (..),
  ScriptContext,
  ScriptHash,
  ScriptInfo (..),
  StakingCredential (..),
  TokenName (..),
  TxId (..),
  TxInInfo,
  TxOut,
  TxOutRef (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)
import Spec.Utils (
  evalSucceeds,
  genByteString,
  inputOutputValidator,
  mkAddressFromByteString,
  mkScriptContext,
  mkScriptCredential,
  mkTxInInfo,
  mkTxOut,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, chooseInteger, forAll, testProperty)

spend :: Term s (PScriptContext :--> PUnit)
spend = StakeValidator.spend

withdraw :: Term s (PScriptContext :--> PUnit)
withdraw = MultiUTxOIndexer.withdraw inputOutputValidator

-- | A combined validator that integrates both staking and spending validation logic.
validator :: Term s (PScriptContext :--> PUnit)
validator = Multivalidator.multivalidator withdraw spend

ownValHash :: ScriptHash
ownValHash = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"

rewardingCred :: Credential
rewardingCred = ScriptCredential ownValHash

inputAddr :: Address
inputAddr =
  let stakeCred = ScriptCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"
   in Address (ScriptCredential ownValHash) (Just (StakingHash stakeCred))

inputOutRef :: TxOutRef
inputOutRef =
  TxOutRef
    (TxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d")
    1

redeemer :: MultiUTxOIndexer.WithdrawRedeemer
redeemer =
  MultiUTxOIndexer.WithdrawRedeemer
    [MultiUTxOIndexer.Indices {inIdx = mkI 0, outIdx = mkI 0}]

badRedeemer :: MultiUTxOIndexer.WithdrawRedeemer
badRedeemer =
  MultiUTxOIndexer.WithdrawRedeemer
    [MultiUTxOIndexer.Indices {inIdx = mkI 0, outIdx = mkI 1}]

-- | A script context for spend transactions, incorporating UTxO details and withdrawal credentials.
spendContext :: TxOutRef -> ScriptContext
spendContext ownRef =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    []
    mempty
    [(rewardingCred, 1)]
    []
    (Redeemer $ PlutusTx.toBuiltinData redeemer)
    (SpendingScript ownRef Nothing)

-- | A script context for withdraw transactions, using input and output UTxOs.
withdrawContext :: MultiUTxOIndexer.WithdrawRedeemer -> ScriptContext
withdrawContext withdrawRedeemer =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    [mkTxOut inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    mempty
    []
    []
    (Redeemer $ PlutusTx.toBuiltinData withdrawRedeemer)
    (RewardingScript rewardingCred)

-- | Unit tests evaluating the correct operation of the validator under various scenarios.
unitTest :: TestTree
unitTest =
  testGroup
    "Multi UTxI Indexer Unit Test"
    [ testEval "Pass - Spend" (validator # pconstant (spendContext inputOutRef))
    , testEvalFail
        "Fail - Spend incorrect out ref"
        (validator # pconstant (spendContext $ TxOutRef (TxId "") 0))
    , testEval "Pass - Withdraw" (validator # pconstant (withdrawContext redeemer))
    , testEvalFail "Fail - Withdraw" (validator # pconstant (withdrawContext badRedeemer))
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawAmount =
  let txOutRef = TxOutRef (TxId txId) 0
      credential = mkScriptCredential valHash
   in mkScriptContext
        [mkTxInInfo txOutRef (mkAddressFromByteString valHash) mempty]
        []
        mempty
        [(credential, withdrawAmount)]
        []
        (Redeemer $ PlutusTx.toBuiltinData ())
        (SpendingScript txOutRef Nothing)

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      txId <- genByteString 64
      valHash <- genByteString 56
      withdrawAmount <- chooseInteger (1, 1_000_000_000)
      pure (txId, valHash, withdrawAmount)
    check (txId, valHash, withdrawAmount) =
      evalSucceeds $ spend # pconstant (mkSpendCtx txId valHash withdrawAmount)

mkInputs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Integer -> [TxInInfo]
mkInputs txId valHash stateTokenSymbol tokenName numPairs = mkInput <$> [0 .. numPairs - 1]
  where
    address = mkAddressFromByteString valHash
    value i =
      singleton (CurrencySymbol "") (TokenName "") (i * 2_000_000)
        <> singleton (CurrencySymbol stateTokenSymbol) (TokenName tokenName) 1
    mkInput i = mkTxInInfo (TxOutRef (TxId txId) i) address (value i)

mkOutputs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Integer -> [TxOut]
mkOutputs valHash stateTokenSymbol tokenName numPairs = mkOutput <$> [0 .. numPairs - 1]
  where
    address = mkAddressFromByteString valHash
    value i =
      singleton (CurrencySymbol "") (TokenName "") (i * 2_000_000)
        <> singleton (CurrencySymbol stateTokenSymbol) (TokenName tokenName) 1
    mkOutput i = mkTxOut address (value i)

mkRedeemer :: Integer -> MultiUTxOIndexer.WithdrawRedeemer
mkRedeemer n =
  MultiUTxOIndexer.WithdrawRedeemer $
    (\i -> MultiUTxOIndexer.Indices (mkI i) (mkI i)) <$> [0 .. n - 1]

mkWithdrawCtx :: BuiltinByteString -> [TxInInfo] -> [TxOut] -> MultiUTxOIndexer.WithdrawRedeemer -> ScriptContext
mkWithdrawCtx valHash inputs outputs withdrawRedeemer =
  let credential = mkScriptCredential valHash
   in mkScriptContext
        inputs
        outputs
        mempty
        []
        []
        (Redeemer $ PlutusTx.toBuiltinData withdrawRedeemer)
        (RewardingScript credential)

prop_withdrawValidator :: Property
prop_withdrawValidator = forAll withdrawInput check
  where
    withdrawInput = do
      stateTokenSymbol <- genByteString 56
      txId <- genByteString 64
      valHash <- genByteString 56
      tokenNameLength <- chooseInt (0, 32)
      tokenName <- genByteString tokenNameLength
      numPairs <- chooseInteger (1, 10)
      pure (stateTokenSymbol, txId, valHash, tokenName, numPairs)
    check (stateTokenSymbol, txId, valHash, tokenName, numPairs) =
      let inputs = mkInputs txId valHash stateTokenSymbol tokenName numPairs
          outputs = mkOutputs valHash stateTokenSymbol tokenName numPairs
          withdrawRedeemer = mkRedeemer numPairs
       in evalSucceeds $
            withdraw # pconstant (mkWithdrawCtx valHash inputs outputs withdrawRedeemer)

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for MultiUTxOIndexer"
    [ testProperty "spend" prop_spendValidator
    , testProperty "withdraw" prop_withdrawValidator
    ]
