{-# LANGUAGE OverloadedStrings  #-}

{- |
Module      : Spec.MerkelizedValidatorSpec
Description : Test suite for the Merkelized Validator functions in a Plutarch smart contract environment.
-}
module Spec.MerkelizedValidatorSpec (
  propertyTests,
  psumOfSquares,
  spendUnitTest,
  withdrawUnitTest,
  withdraw,
) where

import Plutarch.Api.V2 (
  PScriptContext,
  PStakeValidator,
  PStakingCredential (..),
  PValidator,
 )
import Plutarch.Builtin (pasInt, pdataImpl, pforgetData)
import Plutarch.Context (
  buildRewarding',
  buildSpending',
  extraRedeemer,
  withdrawal,
 )
import Plutarch.MerkelizedValidator qualified as MerkelizedValidator
import Plutarch.Num ((#*), (#+))
import Plutarch.Prelude
import Plutarch.Utils (pheadSingleton)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  Credential (..),
  ScriptContext,
  ScriptPurpose (..),
  StakingCredential (..),
  ScriptHash (..),
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
 )

import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Test.QuickCheck (fromPPartial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (forAll, Property, testProperty, chooseInteger, listOf)

import Spec.Utils

-- | Calculates the sum of squares of integers provided as a list of 'PData'.
psumOfSquares :: (PIsListLike list PData, PIsListLike list PInteger) => Term s (PBuiltinList PData :--> list PData)
psumOfSquares =
  plam $ \xs ->
    let result = pfoldl # plam (\y x -> (x #* x) #+ y) # 0 # (pmap # pasInt # xs)
     in psingleton # pdataImpl result

-- | Validates a transaction based on a predefined logic involving a sum less than a threshold value.
spend :: Term s PStakingCredential -> Term s PValidator
spend stakeCred =
  plam $ \x y ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo"] ctx
    txInfoF <- pletFieldsC @'["redeemers"] ctxF.txInfo
    sum' <- pletC $ pheadSingleton #$ MerkelizedValidator.spend stakeCred (pcons # x # (pcons # y # pnil)) txInfoF.redeemers
    sum <- pletC $ pasInt # sum'
    return $
      pif
        (sum #< 42)
        (popaque $ pconstant ())
        perror

-- | Merkelized withdrawal function that validates state transitions based on sum of squares calculation.
withdraw :: Term s PStakeValidator
withdraw = MerkelizedValidator.withdraw psumOfSquares

rewardingCred :: StakingCredential
rewardingCred = StakingHash (ScriptCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8")

stakeCred :: Term s PStakingCredential
stakeCred = pconstant rewardingCred

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

spendCtx :: ScriptContext
spendCtx = spendCtxWithCred rewardingCred withdrawRedeemer

spendCtxWithCred :: StakingCredential -> MerkelizedValidator.WithdrawRedeemer -> ScriptContext
spendCtxWithCred s redeemer =
  buildSpending' $
    mconcat
      [ withdrawal s 1
      , extraRedeemer (Rewarding s) redeemer
      ]

withdrawCtx :: ScriptContext
withdrawCtx = withdrawCtxWithCred rewardingCred

withdrawCtxWithCred :: StakingCredential -> ScriptContext
withdrawCtxWithCred s =
  buildRewarding' $
    mconcat
      [ withdrawal s 0
      ]

-- | Tests the 'spend' function for both successful and failed validation scenarios.
spendUnitTest :: TestTree
spendUnitTest = tryFromPTerm "Merkelized Validator Spend Unit Test" (spend stakeCred) $ do
  testEvalCase
    "Pass - Spend"
    Success
    [ PlutusTx.toData (2 :: Integer)
    , PlutusTx.toData (4 :: Integer)
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Fail - Spend incorrect datum and redeemer"
    Failure
    [ PlutusTx.toData (3 :: Integer)
    , PlutusTx.toData (4 :: Integer)
    , PlutusTx.toData spendCtx
    ]

-- | Tests the 'withdraw' function to ensure correct state transition validations.
withdrawUnitTest :: TestTree
withdrawUnitTest = tryFromPTerm "Merkelized Validator Withdraw Unit Test" withdraw $ do
  testEvalCase
    "Pass - Withdraw"
    Success
    [ PlutusTx.toData withdrawRedeemer
    , PlutusTx.toData withdrawCtx
    ]
  testEvalCase
    "Fail - Withdraw"
    Failure
    [ PlutusTx.toData badWithdrawRedeemer
    , PlutusTx.toData withdrawCtx
    ]

prop_withdrawValidator :: Property
prop_withdrawValidator = forAll withdrawInput check
 where
  withdrawInput = do
    xs <- listOf (chooseInteger (-1_000_000_000, 1_000_000_000))
    bs <- genByteString 56
    return (xs, bs)
  check (xs, bs) =
    let inputState :: ClosedTerm (PBuiltinList PData)
        inputState  = pmap # (plam $ pforgetData . pdata) # pconstant xs in
    let outputState = psumOfSquares # inputState                         in
    let redeemer :: ClosedTerm MerkelizedValidator.PWithdrawRedeemer
        redeemer = pcon $ MerkelizedValidator.PWithdrawRedeemer
                         $ pdcons @"inputState"   # pdata inputState
                        #$ pdcons @"outputState"  # pdata outputState
                         # pdnil                                         in
    let cred = StakingHash (ScriptCredential (ScriptHash bs))            in
    let context :: ClosedTerm PScriptContext
        context = pconstant (withdrawCtxWithCred cred)                   in
    fromPPartial $ withdraw # pforgetData (pdata redeemer) # context

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
 where
  spendInput = do
    x  <- chooseInteger (-4, 4)
    y  <- chooseInteger (-4, 4)
    bs <- genByteString 56
    return (x, y, bs)
  check (x :: Integer, y :: Integer, bs :: BuiltinByteString) =
    let cred = StakingHash (ScriptCredential (ScriptHash bs)) in
    let redeemer = MerkelizedValidator.WithdrawRedeemer
          { inputState = [ PlutusTx.toBuiltinData x, PlutusTx.toBuiltinData y ]
          , outputState = [ PlutusTx.toBuiltinData (x * x + y * y) ]
          }                                                   in
    let context :: ClosedTerm PScriptContext
        context = pconstant (spendCtxWithCred cred redeemer)  in
    let asData = pforgetData . pdata . pconstant              in
    fromPPartial $ spend (pconstant cred) # asData x # asData y # context

propertyTests :: TestTree
propertyTests = testGroup "Property tests for MerkelizedValidator" [ testProperty "withdraw" prop_withdrawValidator
                                                                   , testProperty "spend"    prop_spendValidator
                                                                   ]
