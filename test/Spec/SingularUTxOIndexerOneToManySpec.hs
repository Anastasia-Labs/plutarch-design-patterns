{- |
Module      : Spec.SingularUTxOIndexerOneToManySpec
Description : Test suite for Singular UTxO Indexer in a one-to-many configuration using Plutarch.
-}
module Spec.SingularUTxOIndexerOneToManySpec (
  spend,
  unitTest,
  propertyTest,
) where

import Plutarch.Api.V2 (PScriptContext, PValidator)
import Plutarch.Builtin (pforgetData)
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  withRef,
  withRefIndex,
  withRefTxId,
  withSpendingOutRef,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Prelude
import Plutarch.SingularUTxOIndexerOneToMany qualified as SingularUTxOIndexerOneToMany
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Test.QuickCheck (fromPPartial)
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
import Spec.Utils (genByteString, mkAddressFromByteString)
import Spec.Utils qualified as Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, forAll, testProperty)

-- | A validator that enforces one-to-many UTxO indexing rules for spend transactions.
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

-- | Script context for spending tests, incorporating input and output UTxOs, and other contextual information.
spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

-- | Unit tests that validate the correct operation and rejection scenarios of the spend validator.
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

mkInputUTXO :: TxOutRef -> BuiltinByteString -> CurrencySymbol -> TokenName -> UTXO
mkInputUTXO outRef valHash stateTokenSymbol tokenName =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withValue ((singleton "" "" 2_000_000) <> (singleton stateTokenSymbol tokenName 1))
    , withRef outRef
    ]

mkOutputUTXO :: BuiltinByteString -> CurrencySymbol -> TokenName -> UTXO
mkOutputUTXO valHash stateTokenSymbol tokenName =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withValue ((singleton "" "" 2_000_000) <> (singleton stateTokenSymbol tokenName 1))
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> ScriptContext
mkSpendCtx txId valHash stateTokenSymbol tokenName =
  let txOutRef = TxOutRef (TxId txId) 0
      cs = CurrencySymbol stateTokenSymbol
      tn = TokenName tokenName
   in buildSpending' $
        mconcat
          [ input (mkInputUTXO txOutRef valHash cs tn)
          , output (mkOutputUTXO valHash cs tn)
          , withSpendingOutRef txOutRef
          , withdrawal rewardingCred 1
          ]

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      stateTokenSymbol <- genByteString 56
      txId <- genByteString 64
      valHash <- genByteString 56
      tokenNameLength <- chooseInt (0, 32)
      tokenName <- genByteString tokenNameLength
      return (stateTokenSymbol, txId, valHash, tokenName)
    check (stateTokenSymbol, txId, valHash, tokenName) =
      let context :: ClosedTerm PScriptContext
          context = pconstant $ mkSpendCtx txId valHash stateTokenSymbol tokenName
          redeemer :: ClosedTerm SingularUTxOIndexerOneToMany.PSpendRedeemer
          redeemer = pconstant $ SingularUTxOIndexerOneToMany.SpendRedeemer (mkI 0) [mkI 0]
       in fromPPartial $ spend # (pforgetData . pdata . pconstant) (0 :: Integer) # (pforgetData . pdata) redeemer # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for SingularUTxOIndexerOneToMany"
    [testProperty "spend" prop_spendValidator]
