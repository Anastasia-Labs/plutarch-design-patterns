{- |
Module      : Spec.MerkelizedValidatorSpec
Description : Test suite for the Merkelized Validator functions in a Plutarch smart contract environment.
-}
module Spec.MerkelizedValidatorSpec (
  propertyTest,
  psumOfSquares,
  spendUnitTest,
  withdrawUnitTest,
  withdraw,
) where

import Plutarch.Core.List (pheadSingleton)
import Plutarch.LedgerApi.V3 (
  PCredential,
  PScriptContext (..),
  PTxInfo (..),
 )
import Plutarch.MerkelizedValidator qualified as MerkelizedValidator
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Test.Unit (testEval, testEvalFail)
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  Credential,
  Redeemer (..),
  ScriptContext,
  ScriptInfo (..),
  ScriptPurpose (..),
  TxId (..),
  TxOutRef (..),
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)
import Spec.Utils (
  evalSucceeds,
  genByteString,
  mkScriptContext,
  mkScriptCredential,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInteger, forAll, listOf, testProperty)

-- | Calculates the sum of squares of integers provided as a list of 'PData'.
psumOfSquares :: Term s (PBuiltinList PData :--> PBuiltinList PData)
psumOfSquares =
  plam $ \xs ->
    let result = pfoldl # plam (\total item -> total + pasInt # item * (pasInt # item)) # 0 # xs
     in psingleton # pforgetData (pdata result)

-- | Validates a transaction based on a predefined logic involving a sum less than a threshold value.
spend :: Term s PCredential -> Term s (PInteger :--> PInteger :--> PScriptContext :--> PUnit)
spend stakeCred =
  plam $ \x y ctx -> P.do
    PScriptContext {pscriptContext'txInfo} <- pmatch ctx
    PTxInfo {ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
    sumData <-
      plet $
        pheadSingleton
          # MerkelizedValidator.spend
            stakeCred
            (pcons # pforgetData (pdata x) # (pcons # pforgetData (pdata y) # pnil))
            (pfromData ptxInfo'redeemers)
    pif (pasInt # sumData #< 42) (pconstant ()) perror

-- | Merkelized withdrawal function that validates state transitions based on sum of squares calculation.
withdraw :: Term s (PScriptContext :--> PUnit)
withdraw = MerkelizedValidator.withdraw psumOfSquares

rewardingCred :: Credential
rewardingCred =
  mkScriptCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"

withdrawRedeemer :: MerkelizedValidator.WithdrawRedeemer
withdrawRedeemer =
  MerkelizedValidator.WithdrawRedeemer
    { inputState = [mkI 2, mkI 4]
    , outputState = [mkI 20]
    }

badWithdrawRedeemer :: MerkelizedValidator.WithdrawRedeemer
badWithdrawRedeemer =
  MerkelizedValidator.WithdrawRedeemer
    { inputState = [mkI 2, mkI 7]
    , outputState = [mkI 25]
    }

dummyOutRef :: TxOutRef
dummyOutRef = TxOutRef (TxId "") 0

spendCtxWithCred :: Credential -> MerkelizedValidator.WithdrawRedeemer -> ScriptContext
spendCtxWithCred credential stateRedeemer =
  mkScriptContext
    []
    []
    mempty
    [(credential, 1)]
    [(Rewarding credential, Redeemer $ PlutusTx.toBuiltinData stateRedeemer)]
    (Redeemer $ PlutusTx.toBuiltinData ())
    (SpendingScript dummyOutRef Nothing)

withdrawCtxWithCred :: Credential -> MerkelizedValidator.WithdrawRedeemer -> ScriptContext
withdrawCtxWithCred credential stateRedeemer =
  mkScriptContext
    []
    []
    mempty
    [(credential, 0)]
    []
    (Redeemer $ PlutusTx.toBuiltinData stateRedeemer)
    (RewardingScript credential)

-- | Tests the 'spend' function for both successful and failed validation scenarios.
spendUnitTest :: TestTree
spendUnitTest =
  testGroup
    "Merkelized Validator Spend Unit Test"
    [ testEval
        "Pass - Spend"
        (spend (pconstant rewardingCred) # 2 # 4 # pconstant (spendCtxWithCred rewardingCred withdrawRedeemer))
    , testEvalFail
        "Fail - Spend incorrect datum and redeemer"
        (spend (pconstant rewardingCred) # 3 # 4 # pconstant (spendCtxWithCred rewardingCred withdrawRedeemer))
    ]

-- | Tests the 'withdraw' function to ensure correct state transition validations.
withdrawUnitTest :: TestTree
withdrawUnitTest =
  testGroup
    "Merkelized Validator Withdraw Unit Test"
    [ testEval
        "Pass - Withdraw"
        (withdraw # pconstant (withdrawCtxWithCred rewardingCred withdrawRedeemer))
    , testEvalFail
        "Fail - Withdraw"
        (withdraw # pconstant (withdrawCtxWithCred rewardingCred badWithdrawRedeemer))
    ]

prop_withdrawValidator :: Property
prop_withdrawValidator = forAll withdrawInput check
  where
    withdrawInput = do
      xs <- listOf (chooseInteger (-1_000_000_000, 1_000_000_000))
      bytes <- genByteString 56
      pure (xs, bytes)
    check (xs, bytes) =
      let credential = mkScriptCredential bytes
          stateRedeemer =
            MerkelizedValidator.WithdrawRedeemer
              { inputState = PlutusTx.toBuiltinData <$> xs
              , outputState = [PlutusTx.toBuiltinData $ sum $ fmap (\x -> x * x) xs]
              }
       in evalSucceeds $
            withdraw # pconstant (withdrawCtxWithCred credential stateRedeemer)

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      x <- chooseInteger (-4, 4)
      y <- chooseInteger (-4, 4)
      bytes <- genByteString 56
      pure (x, y, bytes)
    check (x :: Integer, y :: Integer, bytes :: BuiltinByteString) =
      let credential = mkScriptCredential bytes
          stateRedeemer =
            MerkelizedValidator.WithdrawRedeemer
              { inputState = [PlutusTx.toBuiltinData x, PlutusTx.toBuiltinData y]
              , outputState = [PlutusTx.toBuiltinData $ x * x + y * y]
              }
       in evalSucceeds $
            spend (pconstant credential)
              # pconstant x
              # pconstant y
              # pconstant (spendCtxWithCred credential stateRedeemer)

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for MerkelizedValidator"
    [ testProperty "withdraw" prop_withdrawValidator
    , testProperty "spend" prop_spendValidator
    ]
