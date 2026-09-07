{- |
Module      : Spec.StakeValidatorSpec
Description : Test suite for the Stake Validator in a Plutarch smart contract environment.
-}
module Spec.StakeValidatorSpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.LedgerApi.V3 (PCredential, PScriptContext, PTxInfo)
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Unit (testEval, testEvalFail)
import PlutusLedgerApi.V3 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  Redeemer (..),
  ScriptContext,
  ScriptHash,
  ScriptInfo (..),
  StakingCredential (..),
  TxId (..),
  TxOutRef (..),
 )
import PlutusTx qualified
import Spec.Utils (
  evalSucceeds,
  genByteString,
  mkAddressFromByteString,
  mkScriptContext,
  mkScriptCredential,
  mkTxInInfo,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInteger, forAll, testProperty)

-- | Implements the spending logic.
spend :: Term s (PScriptContext :--> PUnit)
spend = StakeValidator.spend

-- | Implements the withdrawal logic.
withdrawLogic :: Term s (PData :--> PCredential :--> PTxInfo :--> PUnit)
withdrawLogic = plam $ \_ _ _ -> pconstant ()

withdraw :: Term s (PScriptContext :--> PUnit)
withdraw = StakeValidator.withdraw withdrawLogic

-- | Core validator that combines spend and withdraw functionalities.
validator :: Term s (PScriptContext :--> PUnit)
validator = Multivalidator.multivalidator withdraw spend

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

-- | Context setup for standard spend tests including necessary UTxO and withdrawal credentials.
spendCtx :: ScriptContext
spendCtx =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr mempty]
    []
    mempty
    [(rewardingCred, 1)]
    []
    (Redeemer $ PlutusTx.toBuiltinData ())
    (SpendingScript inputOutRef Nothing)

spendIncorrectOutRefCtx :: ScriptContext
spendIncorrectOutRefCtx =
  mkScriptContext
    [mkTxInInfo inputOutRef inputAddr mempty]
    []
    mempty
    [(rewardingCred, 1)]
    []
    (Redeemer $ PlutusTx.toBuiltinData ())
    ( SpendingScript
        (TxOutRef "9b029362542b31ffd2340ea85dd8f28e271ff6d2c6dbc95c1e96349c4131a9d1" 0)
        Nothing
    )

-- | Context setup for successful withdrawal tests.
withdrawCtx :: ScriptContext
withdrawCtx =
  mkScriptContext
    []
    []
    mempty
    []
    []
    (Redeemer $ PlutusTx.toBuiltinData ())
    (RewardingScript rewardingCred)

-- | Context setup for withdrawal tests expected to fail.
badWithdrawCtx :: ScriptContext
badWithdrawCtx =
  mkScriptContext
    []
    []
    mempty
    []
    []
    (Redeemer $ PlutusTx.toBuiltinData ())
    (SpendingScript inputOutRef Nothing)

-- | Unit tests to verify the correct behavior and error handling of the validator under various scenarios.
unitTest :: TestTree
unitTest =
  testGroup
    "Stake Validator Unit Test"
    [ testEval "Pass - Withdraw" (validator # pconstant withdrawCtx)
    , testEvalFail "Fail - Withdraw" (validator # pconstant badWithdrawCtx)
    , testEval "Pass - Spend" (validator # pconstant spendCtx)
    , testEvalFail "Fail - Spend" (validator # pconstant spendIncorrectOutRefCtx)
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawAmount =
  let txOutRef = TxOutRef (TxId txId) 0
      credential = mkScriptCredential valHash
   in mkScriptContext
        [mkTxInInfo txOutRef (mkAddressFromByteString valHash) mempty]
        []
        mempty
        [(credential, withdrawAmount)]
        []
        (Redeemer $ PlutusTx.toBuiltinData ())
        (SpendingScript txOutRef Nothing)

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

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for StakeValidator"
    [testProperty "spend" prop_spendValidator]
