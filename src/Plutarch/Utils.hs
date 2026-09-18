{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.Utils (
  PRedeemers,
  PWithdrawals,
  pdropList,
  pelemAtViaDrop,
  punsafeGetWithdrawScriptsRedeemerAt,
) where

import Plutarch.Core.Utils (passert)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PCredential (..),
  PLovelace,
  PRedeemer,
  PScriptHash,
  PScriptPurpose (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC

-- | The transaction's map from script purposes to redeemers.
type PRedeemers = AssocMap.PUnsortedMap PScriptPurpose PRedeemer

-- | The transaction's map from staking credentials to withdrawal amounts.
type PWithdrawals = AssocMap.PUnsortedMap PCredential PLovelace

{- | Drop elements using the protocol version 11 'PLC.DropList' builtin.
Negative counts act as zero; callers requiring rejection must check them.
Apply 'phead' to the result to select an element by its zero-based index.
-}
pdropList ::
  Term s (PInteger :--> PBuiltinList a :--> PBuiltinList a)
pdropList = phoistAcyclic $ pforce $ punsafeBuiltin PLC.DropList

{- | Select an element using 'pdropList' followed by 'phead'.
Negative indices select the first element; an empty result fails at 'phead'.
-}
pelemAtViaDrop ::
  (PIsListLike PBuiltinList a) =>
  Term s PInteger ->
  Term s (PBuiltinList a) ->
  Term s a
pelemAtViaDrop index xs = phead # (pdropList # index # xs)

{- | Retrieve a redeemer by index, requiring the matching script withdrawal purpose.
Negative indices select the first entry. Fails if no entry exists at the
selected position or if its purpose or script hash differs.
The payload is annotated as @a@ without structural validation; callers are
responsible for ensuring it has the expected representation.
-}
punsafeGetWithdrawScriptsRedeemerAt ::
  forall a s.
  Term s PScriptHash ->
  Term s PRedeemers ->
  Term s PInteger ->
  Term s (PAsData a)
punsafeGetWithdrawScriptsRedeemerAt withdrawScriptHash redeemers redeemerIndex = P.do
  PBuiltinPair purposeData redeemerData <-
    pmatch $ pelemAtViaDrop redeemerIndex (pto (pto redeemers))
  let expectedPurpose = pcon $ PRewarding $ pcon $ PScriptCredential $ pdata withdrawScriptHash
  passert
    "Selected redeemer does not match the withdrawal script purpose"
    (pfromData purposeData #== expectedPurpose)
  punsafeCoerce $ pto $ pfromData redeemerData
