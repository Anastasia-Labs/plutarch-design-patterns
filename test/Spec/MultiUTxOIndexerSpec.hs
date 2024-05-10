{- |
Module      : Spec.MultiUTxOIndexerSpec
Description : Test suite for MultiUTxO Indexer validation in a Plutarch-based smart contract setting.
-}
module Spec.MultiUTxOIndexerSpec (
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
import Plutarch.MultiUTxOIndexer qualified as MultiUTxOIndexer
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
