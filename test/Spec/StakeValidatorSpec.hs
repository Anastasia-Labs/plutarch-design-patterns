module Spec.StakeValidatorSpec (
  validator,
  unitTest,
) where

import Plutarch.Api.V2 (PStakeValidator, PStakingCredential, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Context (
  UTXO,
  address,
  buildRewarding',
  buildSpending',
  input,
  withRefIndex,
  withRefTxId,
  withRewarding,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Utils (WrapperRedeemer (..))
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

spend :: Term s PValidator
spend = StakeValidator.spend

withdrawLogic :: Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

withdraw :: Term s PStakeValidator
withdraw = StakeValidator.withdraw withdrawLogic

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

spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      ]

spendIncorrectOutRefCtx :: ScriptContext
spendIncorrectOutRefCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "9b029362542b31ffd2340ea85dd8f28e271ff6d2c6dbc95c1e96349c4131a9d1"
      , withdrawal rewardingCred 1
      ]

withdrawCtx :: ScriptContext
withdrawCtx =
  buildRewarding' $
    mconcat
      [ withRewarding rewardingCred
      ]

badWithdrawCtx :: ScriptContext
badWithdrawCtx =
  buildSpending' $ mconcat []

unitTest :: TestTree
unitTest = tryFromPTerm "Stake Validator Unit Test" validator $ do
  testEvalCase
    "Pass - Withdraw"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData withdrawCtx
    ]
  testEvalCase
    "Fail - Withdraw"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData badWithdrawCtx
    ]
  testEvalCase
    "Pass - Spend"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData (WrapperRedeemer 0)
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Fail - Spend"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData (WrapperRedeemer 0)
    , PlutusTx.toData spendIncorrectOutRefCtx
    ]
