{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Plutarch.StakeValidator (
  spend,
  withdraw,
  validateWithdraw,
  validateWithdrawWithAmount,
  WithdrawalWithAmount (..),
) where

import Plutarch.Core.Utils (passert, ptryOwnInput)
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PLovelace,
  PRedeemer (..),
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Utils (
  PRedeemers,
  PWithdrawals,
  pelemAtViaDrop,
  punsafeGetWithdrawScriptsRedeemerAt,
 )

validateWithdraw ::
  forall a s.
  Term s PScriptHash ->
  Term s PRedeemers ->
  Term s PInteger ->
  Term s (PAsData a)
validateWithdraw = punsafeGetWithdrawScriptsRedeemerAt

-- | Haskell-level result record; constructing it allocates no on-chain record.
data WithdrawalWithAmount a s = WithdrawalWithAmount
  { withdrawalRedeemer :: Term s (PAsData a)
  , withdrawalAmount :: Term s PLovelace
  }

{- | Check the indexed redeemer purpose and withdrawal credential, then pass
the redeemer and amount to the caller. The redeemer payload is not validated.
Negative indices select the first entry, as with 'pelemAtViaDrop'.
-}
validateWithdrawWithAmount ::
  forall a r s.
  Term s PScriptHash ->
  Term s PRedeemers ->
  Term s PInteger ->
  Term s PWithdrawals ->
  Term s PInteger ->
  (WithdrawalWithAmount a s -> Term s r) ->
  Term s r
validateWithdrawWithAmount scriptHash redeemers redeemerIndex withdrawals withdrawalIndex continue = P.do
  -- Bind strictly so the purpose check runs even if the caller ignores the payload.
  redeemer <- plet $ validateWithdraw @a scriptHash redeemers redeemerIndex
  PBuiltinPair credentialData amountData <-
    pmatch $ pelemAtViaDrop withdrawalIndex (pto (pto withdrawals))
  passert
    "Selected withdrawal belongs to a different script"
    (pfromData credentialData #== pcon (PScriptCredential (pdata scriptHash)))
  amount <- plet $ pfromData amountData
  continue $
    WithdrawalWithAmount
      { withdrawalRedeemer = redeemer
      , withdrawalAmount = amount
      }

spend :: Term s (PScriptContext :--> PUnit)
spend =
  plam $ \ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    PSpendingScript ownRef _ <- pmatch pscriptContext'scriptInfo
    PTxInfo {ptxInfo'inputs, ptxInfo'wdrl} <- pmatch pscriptContext'txInfo
    ownInput <- plet $ ptryOwnInput # pfromData ptxInfo'inputs # ownRef
    PTxOut {ptxOut'address} <- pmatch ownInput
    PAddress {paddress'credential} <- pmatch ptxOut'address
    let withdrawals = pto (pto (pfromData ptxInfo'wdrl))
        hasWithdrawal =
          pany
            # plam
              ( \pair ->
                  pmatch pair $ \(PBuiltinPair credentialData _) ->
                    pfromData credentialData #== paddress'credential
              )
            # withdrawals
    pif hasWithdrawal (pconstant ()) perror

withdraw ::
  Term s (PData :--> PCredential :--> PTxInfo :--> PUnit) ->
  Term s (PScriptContext :--> PUnit)
withdraw withdrawLogic =
  plam $ \ctx -> P.do
    PScriptContext
      { pscriptContext'txInfo
      , pscriptContext'redeemer
      , pscriptContext'scriptInfo
      } <-
      pmatch ctx
    PRedeemer redeemer <- pmatch pscriptContext'redeemer
    PRewardingScript stakeCred <- pmatch pscriptContext'scriptInfo
    withdrawLogic # redeemer # stakeCred # pscriptContext'txInfo
