{- |
Module      : Spec.MultiUTxOIndexerOneToManySpec
Description : Test suite for validating UTxO indexation in a multi-validator setup using the Plutarch environment.
-}
module Spec.MultiUTxOIndexerOneToManySpec (
  validator,
  unitTest,
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
  withValue,
  withdrawal,
 )
import Plutarch.MultiUTxOIndexerOneToMany qualified as MultiUTxOIndexerOneToMany
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)
import Spec.Utils qualified as Utils
import Test.Tasty (TestTree)

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
