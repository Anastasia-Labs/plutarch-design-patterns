module Spec.WithdrawalSpec (tests) where

import Cardano.Api qualified as C
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (setReward)
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainT)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.PlutusLedger.V1 (transScriptHash)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Data.List (elemIndex)
import Data.Text qualified as Text
import Plutarch.Core.Eval (evalWithArgsT)
import Plutarch.Core.Scripts (scriptSize, serialiseScriptShort, tryCompile, tryCompileNoTracing)
import Plutarch.Evaluate (applyArguments, evalScriptUnlimited)
import Plutarch.Internal.Term (Config (NoTracing, Tracing), LogLevel (LogInfo), TracingMode (DoTracing))
import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.Prelude
import Plutarch.StakeValidator (WithdrawalWithAmount (..), validateWithdrawWithAmount)
import Plutarch.Utils (PRedeemers, PWithdrawals)
import PlutusCore.Evaluation.Machine.Exception (ErrorWithCause (..), EvaluationError (..))
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import Spec.Context (getScriptContext)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC
import UntypedPlutusCore.Evaluation.Machine.Cek (CekUserError (CekEvaluationFailure))

-- The fixture's witness accepts arbitrary payloads. The helper under test is
-- evaluated separately against the ledger-produced maps, returning both fields.
witness :: C.PlutusScript C.PlutusScriptV3
witness = C.PlutusScriptSerialised $ serialiseScriptShort $ tryCompileNoTracing (plam $ \(_ :: Term _ PData) -> pcon PUnit)

scriptHash :: C.ScriptHash
scriptHash = C.hashScript $ C.PlutusScript C.PlutusScriptV3 witness

fixture :: Integer -> V3.Data -> ExceptT (BalanceTxError C.ConwayEra) (MockchainT C.ConwayEra IO) V3.ScriptContext
fixture amount payload = do
  let credential = C.StakeCredentialByScript scriptHash
      submit tx = tryBalanceAndSubmit mempty Wallet.w1 tx TrailingChange []
  registration <- BuildTx.execBuildTxT $ BuildTx.addConwayRegCertNoWitness credential
  void $ submit registration
  setReward credential $ C.quantityToLovelace $ C.Quantity amount
  withdrawal <- BuildTx.execBuildTxT $ BuildTx.addScriptWithdrawalWithRedeemerFn witness (C.Quantity amount) (const $ PlutusTx.dataToBuiltinData payload)
  tx <- submit withdrawal
  getScriptContext (C.getTxId $ C.getTxBody tx) $ V3.RewardingScript $ V3.ScriptCredential $ transScriptHash scriptHash

probe :: Term s (PAsData PScriptHash :--> PAsData PRedeemers :--> PAsData PInteger :--> PAsData PWithdrawals :--> PAsData PInteger :--> PData)
probe = plam $ \hash redeemers ri withdrawals wi ->
  validateWithdrawWithAmount @PData (pfromData hash) (pfromData redeemers) (pfromData ri) (pfromData withdrawals) (pfromData wi) $
    \WithdrawalWithAmount {withdrawalRedeemer, withdrawalAmount} ->
      plistData # (pcons # pforgetData withdrawalRedeemer # (pcons # pforgetData (pdata withdrawalAmount) # pnil))

arguments :: V3.ScriptContext -> [V3.Data]
arguments ctx =
  [ PlutusTx.toData $ transScriptHash scriptHash
  , PlutusTx.toData tx.txInfoRedeemers
  , V3.I $ indexOf (V3.Rewarding credential) $ map fst $ Map.toList tx.txInfoRedeemers
  , PlutusTx.toData tx.txInfoWdrl
  , V3.I $ indexOf credential $ map fst $ Map.toList tx.txInfoWdrl
  ]
  where
    tx = ctx.scriptContextTxInfo
    credential = V3.ScriptCredential $ transScriptHash scriptHash
    indexOf key = maybe (error "Fixture entry missing") fromIntegral . elemIndex key

tests :: TestTree
tests =
  testGroup
    "validateWithdrawWithAmount"
    [ testCase "returns the ledger withdrawal payload and amount; reports costs" $ checkCase 7 (V3.I 42) True
    , QC.testProperty "accepted withdrawals succeed; changing their redeemer purpose fails" $
        QC.forAll ((,) <$> QC.chooseInteger (1, 1_000_000) <*> QC.oneof [V3.I <$> QC.chooseInteger (-100, 100), pure $ V3.List []]) $
          \(amount, payload) -> QC.ioProperty $ checkCase amount payload False
    ]
  where
    checkCase amount payload report = mockchainSucceeds $ do
      ctx <- failOnError $ fixture amount payload
      liftIO $ do
        let args = arguments ctx
            expected = V3.List [payload, V3.I amount]
        (actual, budget, _) <- either (assertFailure . Text.unpack) pure $ evalWithArgsT NoTracing probe args
        assertEqual "Returned payload and amount" (tryCompileNoTracing (pconstant expected :: Term _ PData)) actual
        when report $ putStrLn $ "NoTracing (testing cost model): " <> show budget <> "; script bytes: " <> show (scriptSize $ tryCompileNoTracing probe)
        -- Helper-level mutation: preserve payloads and indices, change only the
        -- purpose in the extracted redeemer map. Do not rebuild the transaction.
        let V3.ScriptHash bytes = transScriptHash scriptHash
            changed = Map.unsafeFromList [(V3.Minting $ V3.CurrencySymbol bytes, redeemer) | (_, redeemer) <- Map.toList ctx.scriptContextTxInfo.txInfoRedeemers]
            mutatedArgs = take 1 args <> [PlutusTx.toData changed] <> drop 2 args
            traced = tryCompile (Tracing LogInfo DoTracing) probe
        case evalScriptUnlimited $ applyArguments traced mutatedArgs of
          (Left (ErrorWithCause (OperationalError CekEvaluationFailure) _), _, logs) ->
            assertBool ("Unexpected rejection: " <> show logs) $ "Selected redeemer does not match the withdrawal script purpose" `elem` logs
          result -> assertFailure $ "Expected explicit purpose-check failure, got: " <> show result
