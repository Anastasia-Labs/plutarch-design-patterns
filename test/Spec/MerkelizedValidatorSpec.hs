module Spec.MerkelizedValidatorSpec (
  psumOfSquares,
  spendUnitTest,
  withdrawUnitTest,
  withdraw,
) where

import Plutarch.Api.V2 (
  PStakeValidator,
  PStakingCredential (..),
  PValidator,
 )
import Plutarch.Builtin (pasInt, pdataImpl)
import Plutarch.Context (
  buildRewarding',
  extraRedeemer,
  withdrawal,
 )
import Plutarch.MerkelizedValidator qualified as MerkelizedValidator
import Plutarch.Num ((#*), (#+))
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Utils (pheadSingleton)
import PlutusLedgerApi.V2 (
  Credential (..),
  ScriptContext,
  ScriptPurpose (..),
  StakingCredential (..),
 )
import PlutusTx qualified
import PlutusTx.Builtins (mkI)
import Test.Tasty (TestTree)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
 )

psumOfSquares :: (PIsListLike list PData, PIsListLike list PInteger) => Term s (PBuiltinList PData :--> list PData)
psumOfSquares =
  plam $ \xs ->
    let result = pfoldl # plam (\y x -> (x #* x) #+ y) # 0 # (pmap # pasInt # xs)
     in psingleton # pdataImpl result

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

withdraw :: Term s PStakeValidator
withdraw = MerkelizedValidator.withdraw psumOfSquares

rewardingCred :: StakingCredential
rewardingCred = StakingHash (ScriptCredential "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8")

stakeCred :: Term s PStakingCredential
stakeCred =
  pconstant rewardingCred

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
spendCtx =
  buildRewarding' $
    mconcat
      [ withdrawal rewardingCred 1
      , extraRedeemer (Rewarding rewardingCred) withdrawRedeemer
      ]

withdrawCtx :: ScriptContext
withdrawCtx =
  buildRewarding' $
    mconcat
      [ withdrawal rewardingCred 1
      ]

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
    "Fail - Spend"
    Failure
    [ PlutusTx.toData (3 :: Integer)
    , PlutusTx.toData (4 :: Integer)
    , PlutusTx.toData spendCtx
    ]

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
