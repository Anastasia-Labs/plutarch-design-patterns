module Spec.Context (getScriptContext) where

import Cardano.Api qualified as C
import Convex.Class (MockChainState (..), MonadMockchain, getMockChainState)
import Convex.MockChain (getPlutusArgs)
import PlutusLedgerApi.V3 qualified as V3

{- | Retrieve one invocation from accepted transactions in a V3-only scenario.
The ledger constructs the context; this helper only decodes and selects it.
-}
getScriptContext :: (MonadMockchain C.ConwayEra m, MonadFail m) => C.TxId -> V3.ScriptInfo -> m V3.ScriptContext
getScriptContext txId info = do
  state <- getMockChainState
  contexts <- traverse decode $ concatMap snd state.mcsTransactions
  case filter matches contexts of
    [ctx] -> pure ctx
    found -> fail $ "Expected one script context for " <> show (txId, info) <> "; found " <> show (length found)
  where
    matches ctx =
      ctx.scriptContextTxInfo.txInfoId == V3.TxId (V3.toBuiltin $ C.serialiseToRawBytes txId)
        && ctx.scriptContextScriptInfo == info
    decode script = case getPlutusArgs script of
      [arg] -> maybe (fail "Invalid V3 script context") pure $ V3.fromData arg
      _ -> fail "Expected a V3 script with one context argument"
