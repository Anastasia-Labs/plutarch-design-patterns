{- |
Module      : Spec.StakeValidatorSpec
Description : Test suite for the Stake Validator in a Plutarch smart contract environment.
-}
module Spec.StakeValidatorSpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.Api.V2 (PScriptContext, PStakeValidator, PStakingCredential, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pforgetData)
import Plutarch.Context (
  UTXO,
  address,
  buildRewarding',
  buildSpending',
  input,
  withRef,
  withRefIndex,
  withRefTxId,
  withRewarding,
  withSpendingOutRef,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.StakeValidator qualified as StakeValidator
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Test.QuickCheck (fromPPartial)
import Plutarch.Utils (WrapperRedeemer (..))
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  TxId (..),
  TxOutRef (..),
  singleton,
 )
import PlutusTx qualified
import Spec.Utils (genByteString, mkAddressFromByteString, mkStakingHashFromByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInteger, forAll, testProperty)

-- | Implements the spending logic.
spend :: Term s PValidator
spend = StakeValidator.spend

-- | Implements the withdrawal logic.
withdrawLogic :: Term s (PData :--> PStakingCredential :--> PTxInfo :--> PUnit)
withdrawLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

withdraw :: Term s PStakeValidator
withdraw = StakeValidator.withdraw withdrawLogic

-- | Core validator that combines spend and withdraw functionalities.
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

-- | Context setup for standard spend tests including necessary UTxO and withdrawal credentials.
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

-- | Context setup for successful withdrawal tests.
withdrawCtx :: ScriptContext
withdrawCtx =
  buildRewarding' $
    mconcat
      [ withRewarding rewardingCred
      ]

-- | Context setup for withdrawal tests expected to fail.
badWithdrawCtx :: ScriptContext
badWithdrawCtx =
  buildSpending' $ mconcat []

-- | Unit tests to verify the correct behavior and error handling of the validator under various scenarios.
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

mkInputUTxO :: TxOutRef -> BuiltinByteString -> UTXO
mkInputUTxO txOutRef valHash =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withRef txOutRef
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash withdrawAmount =
  let txOutRef = TxOutRef (TxId txId) 0
   in buildSpending' $
        mconcat
          [ input (mkInputUTxO txOutRef valHash)
          , withSpendingOutRef txOutRef
          , withdrawal (mkStakingHashFromByteString valHash) withdrawAmount
          ]

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      txId <- genByteString 64
      valHash <- genByteString 56
      withdrawAmount <- chooseInteger (1, 1_000_000_000)
      return (txId, valHash, withdrawAmount)
    check (txId, valHash, withdrawAmount) =
      let context :: ClosedTerm PScriptContext
          context = pconstant $ mkSpendCtx txId valHash withdrawAmount
          emptyData :: ClosedTerm PData
          emptyData = (pforgetData . pconstantData) (0 :: Integer)
       in fromPPartial $ spend # emptyData # emptyData # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for StakeValidator"
    [testProperty "spend" prop_spendValidator]
