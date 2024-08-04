{- |
Module      : Spec.MultiUTxOIndexerSpec
Description : Test suite for MultiUTxO Indexer validation in a Plutarch-based smart contract setting.
-}
module Spec.MultiUTxOIndexerSpec (
  propertyTest,
  unitTest,
  validator,
) where

import Plutarch.Api.V2 (PStakeValidator, PValidator)
import Plutarch.Context (
  UTXO,
  address,
  buildRewarding',
  buildSpending',
  input,
  output,
  withRefIndex,
  withRefTxId,
  withRewarding,
  withSpendingOutRefId,
  withSpendingUTXO,
  withValue,
  withdrawal,
 )

import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Builtin (pforgetData)
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  ScriptContext,
  ScriptHash (..),
  StakingCredential (..),
  TokenName (..),
  TxId (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)

import Plutarch.MultiUTxOIndexer qualified as MultiUTxOIndexer
import Plutarch.Multivalidator qualified as Multivalidator

import Plutarch.Test.QuickCheck (fromPPartial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, chooseInteger, forAll, testProperty)

import Spec.Utils (genByteString, mkAddressFromByteString, mkStakingHashFromByteString)
import Spec.Utils qualified as Utils

spend :: Term s PValidator
spend = StakeValidator.spend

withdraw :: Term s PStakeValidator
withdraw = MultiUTxOIndexer.withdraw Utils.inputOutputValidator

-- | A combined validator that integrates both staking and spending validation logic.
validator :: Term s PValidator
validator = Multivalidator.multivalidator withdraw spend

ownValHash :: ScriptHash
ownValHash = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"

rewardingCred :: StakingCredential
rewardingCred = StakingHash (ScriptCredential ownValHash)

inputAddr :: Address
inputAddr =
  let stakeCred = ScriptCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"
   in Address (ScriptCredential ownValHash) (Just (StakingHash stakeCred))

inputUTXO :: UTXO
inputUTXO =
  mconcat
    [ address inputAddr
    , withValue (singleton "" "" 4_000_000)
    , withRefTxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
    , withRefIndex 1
    ]

outputUTXO :: UTXO
outputUTXO =
  mconcat
    [ address inputAddr
    , withValue (singleton "" "" 4_000_000)
    ]

-- | A script context for spend transactions, incorporating UTxO details and withdrawal credentials.
spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

redeemer :: MultiUTxOIndexer.WithdrawRedeemer
redeemer =
  MultiUTxOIndexer.WithdrawRedeemer
    [ MultiUTxOIndexer.Indices
        { inIdx = mkI 0
        , outIdx = mkI 0
        }
    ]

badRedeemer :: MultiUTxOIndexer.WithdrawRedeemer
badRedeemer =
  MultiUTxOIndexer.WithdrawRedeemer
    [ MultiUTxOIndexer.Indices
        { inIdx = mkI 0
        , outIdx = mkI 1
        }
    ]

-- | A script context for withdraw transactions, using input and output UTxOs.
withdrawCtx :: ScriptContext
withdrawCtx =
  buildRewarding' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withRewarding rewardingCred
      ]

-- | Unit tests evaluating the correct operation of the validator under various scenarios.
unitTest :: TestTree
unitTest = tryFromPTerm "Multi UTxI Indexer Unit Test" validator $ do
  testEvalCase
    "Pass - Spend"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData redeemer
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Fail - Spend incorrect redeemer"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData ()
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Pass - Withdraw"
    Success
    [ PlutusTx.toData redeemer
    , PlutusTx.toData withdrawCtx
    ]
  testEvalCase
    "Fail - Withdraw"
    Failure
    [ PlutusTx.toData badRedeemer
    , PlutusTx.toData withdrawCtx
    ]

mkInputUTxO :: BuiltinByteString -> BuiltinByteString -> UTXO
mkInputUTxO txId valHash =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withRefTxId (TxId txId)
    , withRefIndex 0
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawAmount =
  let spendingUTxO = mkInputUTxO txId valHash
   in buildSpending' $
        mconcat
          [ input spendingUTxO
          , withSpendingUTXO spendingUTxO
          , withdrawal (mkStakingHashFromByteString valHash) withdrawAmount
          ]

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      txId <- genByteString 64
      valHash <- genByteString 56
      withdrawAmount <- chooseInteger (1, 1_000_000_000)
      return (txId, valHash, withdrawAmount)
    check (txId, valHash, withdrawAmount) =
      let context :: ClosedTerm PScriptContext
          context = pconstant (mkSpendCtx txId valHash withdrawAmount)
          emptyByteString :: ClosedTerm PData
          emptyByteString = (pforgetData . pdata . phexByteStr) ""
       in fromPPartial $ spend # emptyByteString # emptyByteString # context

mkInputs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Integer -> [UTXO]
mkInputs txId valHash stateTokenSymbol tokenName numPairs = mkInput <$> [0 .. (numPairs - 1)]
  where
    mkInput i =
      mconcat
        [ address (mkAddressFromByteString valHash)
        , withValue ((singleton "" "" (i * 2_000_000)) <> (singleton (CurrencySymbol stateTokenSymbol) (TokenName tokenName) 1))
        , withRefTxId (TxId txId)
        , withRefIndex i
        ]

mkOutputs :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Integer -> [UTXO]
mkOutputs valHash stateTokenSymbol tokenName numPairs = mkOutput <$> [0 .. (numPairs - 1)]
  where
    mkOutput i =
      mconcat
        [ address (mkAddressFromByteString valHash)
        , withValue ((singleton "" "" (i * 2_000_000)) <> (singleton (CurrencySymbol stateTokenSymbol) (TokenName tokenName) 1))
        ]

mkRedeemer :: Integer -> MultiUTxOIndexer.WithdrawRedeemer
mkRedeemer n =
  MultiUTxOIndexer.WithdrawRedeemer $ (\i -> MultiUTxOIndexer.Indices (mkI i) (mkI i)) <$> [0 .. (n - 1)]

mkWithdrawCtx :: BuiltinByteString -> [UTXO] -> [UTXO] -> ScriptContext
mkWithdrawCtx valHash inputUTxOs outputUTxOs =
  buildRewarding' $
    mconcat
      [ mconcat $ input <$> inputUTxOs
      , mconcat $ output <$> outputUTxOs
      , withRewarding (mkStakingHashFromByteString valHash)
      ]

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
      return (stateTokenSymbol, txId, valHash, tokenName, numPairs)
    check (stateTokenSymbol, txId, valHash, tokenName, numPairs) =
      let inputs = mkInputs txId valHash stateTokenSymbol tokenName numPairs
          outputs = mkOutputs valHash stateTokenSymbol tokenName numPairs
          redeemer :: ClosedTerm MultiUTxOIndexer.PWithdrawRedeemer
          redeemer = pconstant (mkRedeemer numPairs)
          context :: ClosedTerm PScriptContext
          context = pconstant (mkWithdrawCtx valHash inputs outputs)
       in fromPPartial $ withdraw # pforgetData (pdata redeemer) # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for MultiUTxOIndexer"
    [ testProperty "spend" prop_spendValidator
    , testProperty "withdraw" prop_withdrawValidator
    ]
