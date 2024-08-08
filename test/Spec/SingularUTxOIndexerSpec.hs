{- |
Module      : Spec.SingularUTxOIndexerSpec
Description : Test suite for Singular UTxO Indexer validation in Plutarch, focusing on single input-output pair indexing.
-}
module Spec.SingularUTxOIndexerSpec (
  spend,
  unitTest,
  propertyTest,
) where

import Plutarch.Api.V2 (PScriptContext, PValidator)
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
import Plutarch.SingularUTxOIndexer qualified as SingularUTxOIndexer
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
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

import Plutarch.Test.QuickCheck (fromPPartial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, forAll, testProperty)

import Plutarch.Builtin (pforgetData)
import Spec.Utils (genByteString, mkAddressFromByteString)
import Spec.Utils qualified as Utils

-- | Implements a validator that enforces specific UTxO spending rules using input-output pair indexing.
spend :: Term s PValidator
spend = SingularUTxOIndexer.spend Utils.inputOutputValidator

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

-- | Redeemer configuration for the validator, specifying the correct indexing of input and output UTxOs.
redeemer :: SingularUTxOIndexer.SpendRedeemer
redeemer =
  SingularUTxOIndexer.SpendRedeemer
    { inIdx = mkI 0
    , outIdx = mkI 0
    }

-- | A misconfigured redeemer that is expected to cause the validator to fail, demonstrating error handling.
badRedeemer :: SingularUTxOIndexer.SpendRedeemer
badRedeemer =
  SingularUTxOIndexer.SpendRedeemer
    { inIdx = mkI 0
    , outIdx = mkI 1
    }

-- | Context setup for spending tests, incorporating input and output UTxOs, withdrawal credentials.
spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , output outputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

-- | Unit tests to verify the validator's functionality under both correct and incorrect scenarios.
unitTest :: TestTree
unitTest = tryFromPTerm "Singular UTxO Indexer Unit Test" spend $ do
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
          redeemer :: ClosedTerm SingularUTxOIndexer.PSpendRedeemer
          redeemer = pconstant $ SingularUTxOIndexer.SpendRedeemer (mkI 0) (mkI 0)
       in fromPPartial $ spend # (pforgetData . pdata . pconstant) (0 :: Integer) # (pforgetData . pdata) redeemer # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for SingularUTxOIndexer"
    [testProperty "spend" prop_spendValidator]
