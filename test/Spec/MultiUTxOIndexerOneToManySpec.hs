{- |
Module      : Spec.MultiUTxOIndexerOneToManySpec
Description : Test suite for validating UTxO indexation in a multi-validator setup using the Plutarch environment.
-}
module Spec.MultiUTxOIndexerOneToManySpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.Api.V2 (PScriptContext, PStakeValidator, PValidator)
import Plutarch.Context (
  UTXO,
  address,
  buildRewarding',
  buildSpending',
  input,
  output,
  withRef,
  withRefIndex,
  withRefTxId,
  withRewarding,
  withSpendingOutRef,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )

import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  TokenName (..),
  TxId (..),
  TxOutRef (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)

import Plutarch.MultiUTxOIndexerOneToMany qualified as MultiUTxOIndexerOneToMany
import Plutarch.Multivalidator qualified as Multivalidator

import Plutarch.Builtin (pforgetData)
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)

import Plutarch.Test.QuickCheck (fromPPartial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, chooseInteger, forAll, testProperty)

import Spec.Utils (genByteString, mkAddressFromByteString, mkStakingHashFromByteString)
import Spec.Utils qualified as Utils

-- | Handles the spend logic using the basic Stake Validator.
spend :: Term s PValidator
spend = StakeValidator.spend

-- | Handles withdrawal logic with additional indexing validations for multi-UTxO scenarios.
withdraw :: Term s PStakeValidator
withdraw = MultiUTxOIndexerOneToMany.withdraw Utils.inputValidator Utils.inputOutputValidator Utils.collectiveOutputValidator

-- | Combines staking and spending validation into a single composite validator.
validator :: Term s PValidator
validator = Multivalidator.multivalidator withdraw spend

ownValHash :: ScriptHash
ownValHash = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"

rewardingCred :: StakingCredential
rewardingCred = StakingHash (ScriptCredential ownValHash)

inputAddr :: Address
inputAddr =
  let stakeCred = ScriptCredential "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"
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

-- | Context setup for spend transactions including mock UTxOs and withdrawal.
spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

redeemer :: MultiUTxOIndexerOneToMany.WithdrawRedeemer
redeemer =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer
    [ MultiUTxOIndexerOneToMany.Indices
        { inIdx = mkI 0
        , outIdx = [mkI 0]
        }
    ]

badRedeemer :: MultiUTxOIndexerOneToMany.WithdrawRedeemer
badRedeemer =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer
    [ MultiUTxOIndexerOneToMany.Indices
        { inIdx = mkI 0
        , outIdx = [mkI 1]
        }
    ]

-- | Context setup for withdrawal transactions, integrating input and output UTxOs.
withdrawCtx :: ScriptContext
withdrawCtx =
  buildRewarding' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withRewarding rewardingCred
      ]

-- | Primary unit tests for validating the correct and incorrect behaviors of spend and withdraw functions.
unitTest :: TestTree
unitTest = tryFromPTerm "Multi UTxO Indexer One To Many Unit Test" validator $ do
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

mkInputUTXO :: TxOutRef -> BuiltinByteString -> UTXO
mkInputUTXO outRef valHash =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withValue (singleton "" "" 0)
    , withRef outRef
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawalAmount =
  let outRef = TxOutRef (TxId txId) 0
   in buildSpending' $
        mconcat
          [ input (mkInputUTXO outRef valHash)
          , withSpendingOutRef outRef
          , withdrawal (mkStakingHashFromByteString valHash) withdrawalAmount
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

mkRedeemer :: Integer -> MultiUTxOIndexerOneToMany.WithdrawRedeemer
mkRedeemer n =
  MultiUTxOIndexerOneToMany.WithdrawRedeemer $ (\i -> MultiUTxOIndexerOneToMany.Indices (mkI i) [(mkI i)]) <$> [0 .. (n - 1)]

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
          redeemer :: ClosedTerm MultiUTxOIndexerOneToMany.PWithdrawRedeemer
          redeemer = pconstant (mkRedeemer numPairs)
          context :: ClosedTerm PScriptContext
          context = pconstant (mkWithdrawCtx valHash inputs outputs)
       in fromPPartial $ withdraw # pforgetData (pdata redeemer) # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for MultiUTxOIndexerOneToMany"
    [ testProperty "spend" prop_spendValidator
    , testProperty "withdraw" prop_withdrawValidator
    ]
