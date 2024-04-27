module Plutarch.Multivalidator (
  multivalidator,
) where

import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Builtin (pasConstr)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

multivalidator ::
  Term s (PData :--> PScriptContext :--> POpaque) ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque) ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
multivalidator mintingPolicy spendingValidator = plam $ \redeemerOrDatum scriptContextOrRedeemer -> P.do
  let constrIndex = pfstBuiltin #$ pasConstr # scriptContextOrRedeemer
  pif
    (0 #< constrIndex)
    (spendingValidator # redeemerOrDatum # scriptContextOrRedeemer)
    (punsafeCoerce $ mintingPolicy # redeemerOrDatum # punsafeCoerce scriptContextOrRedeemer)
