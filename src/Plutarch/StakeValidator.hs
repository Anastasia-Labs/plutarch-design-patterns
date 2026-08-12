module Plutarch.StakeValidator (
  spend,
  withdraw,
) where

import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential,
  PRedeemer (..),
  PScriptContext (..),
  PScriptInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Utils (ptryOwnInput)

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
