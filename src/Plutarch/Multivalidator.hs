module Plutarch.Multivalidator (
  multivalidator,
) where

import Plutarch.LedgerApi.V3 (
  PScriptContext (..),
  PScriptInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

multivalidator ::
  Term s (PScriptContext :--> a) ->
  Term s (PScriptContext :--> a) ->
  Term s (PScriptContext :--> a)
multivalidator mintingPolicy spendingValidator =
  plam $ \ctx -> P.do
    PScriptContext {pscriptContext'scriptInfo} <- pmatch ctx
    pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ _ -> spendingValidator # ctx
      _ -> mintingPolicy # ctx
