{- |
Module      : Spec.TxLevelMinterSpec
Description : Test suite for a transaction-level minter validator in a Plutarch smart contract environment.
-}
module Spec.TxLevelMinterSpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PTxInfo)
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.Test.Unit (testEval, testEvalFail)
import Plutarch.TxLevelMinter (WrapperRedeemer (..))
import Plutarch.TxLevelMinter qualified as TxLevelMinter
import PlutusLedgerApi.V3 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  Redeemer (..),
  ScriptContext,
  ScriptHash (..),
  ScriptInfo (..),
  StakingCredential (..),
  TokenName (..),
  TxId (..),
  TxOutRef (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Spec.Utils (
  evalSucceeds,
  genByteString,
  mkAddressFromByteString,
  mkScriptContext,
  mkTxInInfo,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInteger, forAll, testProperty)

-- | Implements the spending logic, including validation of the custom token 'BEACON'.
spend :: Term s (PScriptContext :--> PUnit)
spend = TxLevelMinter.spend # pconstant (TokenName "BEACON")

-- | Simple minting logic that just returns unit (no actual validation logic included for simplicity).
mintLogic :: Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit)
mintLogic = plam $ \_ _ _ -> pconstant ()

-- | Minting policy that encapsulates the minting behavior.
mintingPolicy :: Term s (PScriptContext :--> PUnit)
mintingPolicy = TxLevelMinter.mint mintLogic

-- | Core validator that combines spending and minting functionalities into a single validator logic.
validator :: Term s (PScriptContext :--> PUnit)
validator = Multivalidator.multivalidator mintingPolicy spend

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

mintedValue :: Value
mintedValue =
  singleton
    ownCurrencySymbol
    (TokenName "BEACON")
    1

ownCurrencySymbol :: CurrencySymbol
ownCurrencySymbol =
  case ownValHash of
    ScriptHash hash -> CurrencySymbol hash

-- | Context for spend validation, including input, output, withdrawal of staking credentials, and minting of a token.
spendContext :: WrapperRedeemer -> ScriptContext
spendContext spendRedeemer =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr (singleton (CurrencySymbol "") (TokenName "") 4_000_000)]
    []
    mintedValue
    [(rewardingCred, 1)]
    []
    (Redeemer $ PlutusTx.toBuiltinData spendRedeemer)
    (SpendingScript inputOutRef Nothing)

-- | Context for validating minting actions, configured to handle the minting of the custom token.
mintCtx :: ScriptContext
mintCtx =
  mkScriptContext
    []
    []
    mintedValue
    []
    []
    (Redeemer $ PlutusTx.toBuiltinData ())
    (MintingScript ownCurrencySymbol)

-- | Unit tests to ensure the validator behaves as expected under various scenarios including correct and incorrect token minting.
unitTest :: TestTree
unitTest =
  testGroup
    "Tx Level Minter Unit Test"
    [ testEval "Pass - Spend" (validator # pconstant (spendContext $ WrapperRedeemer 0))
    , testEvalFail
        "Fail - Spend incorrect index input UTXO"
        (validator # pconstant (spendContext $ WrapperRedeemer 1))
    , testEval "Pass - Mint" (validator # pconstant mintCtx)
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash mintAmount =
  let txOutRef = TxOutRef (TxId txId) 0
      mintValue = singleton (CurrencySymbol valHash) (TokenName "BEACON") mintAmount
   in mkScriptContext
        [mkTxInInfo txOutRef (mkAddressFromByteString valHash) mempty]
        []
        mintValue
        []
        []
        (Redeemer $ PlutusTx.toBuiltinData $ WrapperRedeemer 0)
        (SpendingScript txOutRef Nothing)

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      txId <- genByteString 64
      valHash <- genByteString 56
      mintAmount <- chooseInteger (1, 1_000_000_000)
      pure (txId, valHash, mintAmount)
    check (txId, valHash, mintAmount) =
      evalSucceeds $ validator # pconstant (mkSpendCtx txId valHash mintAmount)

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for TxLevelMinter"
    [testProperty "spend" prop_spendValidator]
