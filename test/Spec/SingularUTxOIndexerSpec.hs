{- |
Module      : Spec.SingularUTxOIndexerSpec
Description : Test suite for Singular UTxO Indexer validation in Plutarch, focusing on single input-output pair indexing.
-}
module Spec.SingularUTxOIndexerSpec (
  spend,
  unitTest,
  propertyTest,
) where

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude
import Plutarch.SingularUTxOIndexer qualified as SingularUTxOIndexer
import Plutarch.Test.Unit (testEvalEqual, testEvalFail)
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
  mkTxInInfo,
  mkTxOut,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInt, forAll, testProperty)

-- | Implements a validator that enforces specific UTxO spending rules using input-output pair indexing.
spend :: Term s (PScriptContext :--> PUnit)
spend = SingularUTxOIndexer.spend inputOutputValidator

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
spendContext :: SingularUTxOIndexer.SpendRedeemer -> ScriptContext
spendContext spendRedeemer =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    [mkTxOut inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    mempty
    [(rewardingCred, 1)]
    []
    (Redeemer $ PlutusTx.toBuiltinData spendRedeemer)
    (SpendingScript inputOutRef Nothing)

-- | Unit tests to verify the validator's functionality under both correct and incorrect scenarios.
unitTest :: TestTree
unitTest =
  testGroup
    "Singular UTxO Indexer Unit Test"
    [ testEvalEqual "Pass" (spend # pconstant (spendContext redeemer)) (pconstant ())
    , testEvalFail "Fail - Incorrect redeemer" (spend # pconstant (spendContext badRedeemer))
    , testEvalFail
        "Fail - Input output predicate"
        ( SingularUTxOIndexer.spend (plam $ \_ _ -> pcon PFalse)
            # pconstant (spendContext redeemer)
        )
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> ScriptContext
mkSpendCtx txId valHash stateTokenSymbol tokenName =
  let txOutRef = TxOutRef (TxId txId) 0
      address = mkAddressFromByteString valHash
      value =
        singleton (CurrencySymbol "") (TokenName "") 2_000_000
          <> singleton (CurrencySymbol stateTokenSymbol) (TokenName tokenName) 1
      spendRedeemer = SingularUTxOIndexer.SpendRedeemer (mkI 0) (mkI 0)
   in mkScriptContext
        [mkTxInInfo txOutRef address value]
        [mkTxOut address value]
        mempty
        [(rewardingCred, 1)]
        []
        (Redeemer $ PlutusTx.toBuiltinData spendRedeemer)
        (SpendingScript txOutRef Nothing)

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      stateTokenSymbol <- genByteString 56
      txId <- genByteString 64
      valHash <- genByteString 56
      tokenNameLength <- chooseInt (0, 32)
      tokenName <- genByteString tokenNameLength
      pure (stateTokenSymbol, txId, valHash, tokenName)
    check (stateTokenSymbol, txId, valHash, tokenName) =
      evalSucceeds $
        spend # pconstant (mkSpendCtx txId valHash stateTokenSymbol tokenName)

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for SingularUTxOIndexer"
    [testProperty "spend" prop_spendValidator]
