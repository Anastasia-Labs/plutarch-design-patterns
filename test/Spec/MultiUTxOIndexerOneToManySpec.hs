{- |
Module      : Spec.MultiUTxOIndexerOneToManySpec
Description : Test suite for validating UTxO indexation in a multi-validator setup using the Plutarch environment.
-}
module Spec.MultiUTxOIndexerOneToManySpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.MultiUTxOIndexerOneToMany qualified as MultiUTxOIndexerOneToMany
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
  collectiveOutputValidator,
  evalSucceeds,
  genByteString,
  inputOutputValidator,
  inputValidator,
  mkAddressFromByteString,
  mkScriptContext,
  mkScriptCredential,
  mkTxInInfo,
  mkTxOut,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, chooseInteger, forAll, testProperty)

-- | Handles the spend logic using the basic Stake Validator.
spend :: Term s (PScriptContext :--> PUnit)
spend = StakeValidator.spend

-- | Handles withdrawal logic with additional indexing validations for multi-UTxO scenarios.
withdraw :: Term s (PScriptContext :--> PUnit)
withdraw =
  MultiUTxOIndexerOneToMany.withdraw
    inputValidator
    inputOutputValidator
    collectiveOutputValidator

-- | Combines staking and spending validation into a single composite validator.
validator :: Term s (PScriptContext :--> PUnit)
validator = Multivalidator.multivalidator withdraw spend

ownValHash :: ScriptHash
ownValHash = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"

rewardingCred :: Credential
rewardingCred = ScriptCredential ownValHash

inputAddr :: Address
inputAddr =
  let stakeCred = ScriptCredential ownValHash
   in Address (ScriptCredential ownValHash) (Just (StakingHash stakeCred))

inputOutRef :: TxOutRef
inputOutRef =
  TxOutRef
    (TxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d")
    1

redeemer :: MultiUTxOIndexerOneToMany.WithdrawRedeemer
redeemer =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer
    [MultiUTxOIndexerOneToMany.Indices {inIdx = mkI 0, outIdx = [mkI 0]}]

badRedeemer :: MultiUTxOIndexerOneToMany.WithdrawRedeemer
badRedeemer =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer
    [MultiUTxOIndexerOneToMany.Indices {inIdx = mkI 0, outIdx = [mkI 1]}]

-- | Context setup for spend transactions including mock UTxOs and withdrawal.
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

-- | Context setup for withdrawal transactions, integrating input and output UTxOs.
withdrawContext :: MultiUTxOIndexerOneToMany.WithdrawRedeemer -> ScriptContext
withdrawContext withdrawRedeemer =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    [mkTxOut inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    mempty
    []
    []
    (Redeemer $ PlutusTx.toBuiltinData withdrawRedeemer)
    (RewardingScript rewardingCred)

-- | Primary unit tests for validating the correct and incorrect behaviors of spend and withdraw functions.
unitTest :: TestTree
unitTest =
  testGroup
    "Multi UTxO Indexer One To Many Unit Test"
    [ testEval "Pass - Spend" (validator # pconstant (spendContext inputOutRef))
    , testEvalFail
        "Fail - Spend incorrect out ref"
        (validator # pconstant (spendContext $ TxOutRef (TxId "") 0))
    , testEval "Pass - Withdraw" (validator # pconstant (withdrawContext redeemer))
    , testEvalFail "Fail - Withdraw" (validator # pconstant (withdrawContext badRedeemer))
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawalAmount =
  let outRef = TxOutRef (TxId txId) 0
      credential = mkScriptCredential valHash
   in mkScriptContext
        [mkTxInInfo outRef (mkAddressFromByteString valHash) (singleton (CurrencySymbol "") (TokenName "") 0)]
        []
        mempty
        [(credential, withdrawalAmount)]
        []
        (Redeemer $ PlutusTx.toBuiltinData ())
        (SpendingScript outRef Nothing)

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

mkRedeemer :: Integer -> MultiUTxOIndexerOneToMany.WithdrawRedeemer
mkRedeemer n =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer $
    (\i -> MultiUTxOIndexerOneToMany.Indices (mkI i) [mkI i]) <$> [0 .. n - 1]

mkWithdrawCtx :: BuiltinByteString -> [TxInInfo] -> [TxOut] -> MultiUTxOIndexerOneToMany.WithdrawRedeemer -> ScriptContext
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
    "Property tests for MultiUTxOIndexerOneToMany"
    [ testProperty "spend" prop_spendValidator
    , testProperty "withdraw" prop_withdrawValidator
    ]
