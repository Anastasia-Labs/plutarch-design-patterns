{- |
Module      : Spec.TxLevelMinterSpec
Description : Test suite for a transaction-level minter validator in a Plutarch smart contract environment.
-}
module Spec.TxLevelMinterSpec (
  validator,
  unitTest,
  propertyTest,
) where

import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PScriptContext, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pforgetData)
import Plutarch.Context (
  UTXO,
  address,
  buildMinting',
  buildSpending',
  input,
  mint,
  withRef,
  withRefIndex,
  withRefTxId,
  withSpendingOutRef,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Test.QuickCheck (fromPPartial)
import Plutarch.TxLevelMinter qualified as TxLevelMinter
import Plutarch.Utils (WrapperRedeemer (..))
import PlutusLedgerApi.V2 (
  Address (..),
  BuiltinByteString,
  Credential (..),
  CurrencySymbol (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  TxId (..),
  TxOutRef (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Spec.Utils (genByteString, mkAddressFromByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, chooseInteger, forAll, testProperty)

-- | Implements the spending logic, including validation of the custom token 'BEACON'.
spend :: Term s PValidator
spend = phoistAcyclic $
  plam $ \_ redeemer ctx -> unTermCont $ do
    return (popaque $ TxLevelMinter.spend # pconstant "BEACON" # redeemer # ctx)

-- | Simple minting logic that just returns unit (no actual validation logic included for simplicity).
mintLogic :: Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit)
mintLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

-- | Minting policy that encapsulates the minting behavior.
mintingPolicy :: Term s PMintingPolicy
mintingPolicy = TxLevelMinter.mint mintLogic

-- | Core validator that combines spending and minting functionalities into a single validator logic.
validator :: Term s PValidator
validator = Multivalidator.multivalidator mintingPolicy spend

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

mintedValue :: Value
mintedValue = singleton "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45" "BEACON" 1

-- | Context for spend validation, including input, output, withdrawal of staking credentials, and minting of a token.
spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      , mint mintedValue
      ]

-- | Context for validating minting actions, configured to handle the minting of the custom token.
mintCtx :: ScriptContext
mintCtx =
  buildMinting' $
    mconcat
      [ mint mintedValue
      ]

-- | Unit tests to ensure the validator behaves as expected under various scenarios including correct and incorrect token minting.
unitTest :: TestTree
unitTest = tryFromPTerm "Tx Level Minter Unit Test" validator $ do
  testEvalCase
    "Pass - Spend"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData (WrapperRedeemer 0)
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Fail - Spend incorrect index input UTXO"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData (WrapperRedeemer 1)
    , PlutusTx.toData spendCtx
    ]
  testEvalCase
    "Pass - Mint"
    Success
    [ PlutusTx.toData ()
    , PlutusTx.toData mintCtx
    ]

mkInputUTxO :: TxOutRef -> BuiltinByteString -> UTXO
mkInputUTxO txOutRef valHash =
  mconcat
    [ address (mkAddressFromByteString valHash)
    , withRef txOutRef
    ]

mkSpendCtx :: BuiltinByteString -> BuiltinByteString -> Integer -> ScriptContext
mkSpendCtx txId valHash mintAmount =
  let txOutRef = TxOutRef (TxId txId) 0
   in buildSpending' $
        mconcat
          [ input (mkInputUTxO txOutRef valHash)
          , withSpendingOutRef txOutRef
          , mint (singleton (CurrencySymbol valHash) "BEACON" mintAmount)
          ]

prop_spendValidator :: Property
prop_spendValidator = forAll spendInput check
  where
    spendInput = do
      txId <- genByteString 64
      valHash <- genByteString 56
      mintAmount <- chooseInteger (1, 1_000_000_000)
      return (txId, valHash, mintAmount)
    check (txId, valHash, mintAmount) =
      let context :: ClosedTerm PScriptContext
          context = pconstant $ mkSpendCtx txId valHash mintAmount
          emptyData :: ClosedTerm PData
          emptyData = (pforgetData . pconstantData) (0 :: Integer)
       in fromPPartial $ validator # emptyData # (pforgetData . pconstantData . WrapperRedeemer) 0 # context

propertyTest :: TestTree
propertyTest =
  testGroup
    "Property tests for TxLevelMinter"
    [testProperty "spend" prop_spendValidator]
