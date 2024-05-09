module Spec.TxLevelMinterSpec (
  validator,
  unitTest,
) where

import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Context (
  UTXO,
  address,
  buildMinting',
  buildSpending',
  input,
  mint,
  withRefIndex,
  withRefTxId,
  withSpendingOutRefId,
  withValue,
  withdrawal,
 )
import Plutarch.Multivalidator qualified as Multivalidator
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.TxLevelMinter qualified as TxLevelMinter
import Plutarch.Utils (WrapperRedeemer (..))
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  ScriptContext,
  ScriptHash,
  StakingCredential (..),
  Value,
  singleton,
 )
import PlutusTx qualified
import Test.Tasty (TestTree)

spend :: Term s PValidator
spend = phoistAcyclic $
  plam $ \_ redeemer ctx -> unTermCont $ do
    return (popaque $ TxLevelMinter.spend # pconstant "BEACON" # redeemer # ctx)

mintLogic :: Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit)
mintLogic =
  plam $ \_ _ _ -> unTermCont $ do
    pure $ pconstant ()

mintingPolicy :: Term s PMintingPolicy
mintingPolicy = TxLevelMinter.mint mintLogic

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

spendCtx :: ScriptContext
spendCtx =
  buildSpending' $
    mconcat
      [ input inputUTXO
      , withSpendingOutRefId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
      , withdrawal rewardingCred 1
      , mint mintedValue
      ]

mintCtx :: ScriptContext
mintCtx =
  buildMinting' $
    mconcat
      [ mint mintedValue
      ]

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
