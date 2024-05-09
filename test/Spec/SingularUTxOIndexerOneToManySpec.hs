module Spec.SingularUTxOIndexerOneToManySpec (
  spend,
  unitTest,
) where

import Plutarch.Api.V2 (PValidator)
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Prelude
import Plutarch.SingularUTxOIndexerOneToMany qualified as SingularUTxOIndexerOneToMany
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
spend = SingularUTxOIndexerOneToMany.spend Utils.inputValidator Utils.inputOutputValidator Utils.collectiveOutputValidator

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

redeemer :: SingularUTxOIndexerOneToMany.SpendRedeemer
redeemer =
  SingularUTxOIndexerOneToMany.SpendRedeemer
    { inIx = mkI 0
    , outIxs = [mkI 0]
    }

badRedeemer :: SingularUTxOIndexerOneToMany.SpendRedeemer
badRedeemer =
  SingularUTxOIndexerOneToMany.SpendRedeemer
    { inIx = mkI 0
    , outIxs = [mkI 1]
    }

spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "Singular UTxO Indexer One To Many Unit Test" spend $ do
  testEvalCase
    "Pass"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData redeemer
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Fail - Incorrect redeemer"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData badRedeemer
    , PlutusTx.toData spendCtx
    ]
