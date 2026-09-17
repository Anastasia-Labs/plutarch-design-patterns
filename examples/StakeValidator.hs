{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module StakeValidator (validator, PSpendRedeemer (..)) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (passert)
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PRedeemer (..),
  PScriptContext (..),
  PScriptInfo (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.StakeValidator (WithdrawalWithAmount (..), validateWithdrawWithAmount)
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Utils (pelemAtViaDrop)

{- | Encoded as a Data list containing the input index, withdrawal-redeemer
index, and withdrawal-map index, in that order.
-}
data PSpendRedeemer (s :: S) = PSpendRedeemer
  { pspendRedeemer'inputIndex :: Term s (PAsData PInteger)
  , pspendRedeemer'withdrawalRedeemerIndex :: Term s (PAsData PInteger)
  , pspendRedeemer'withdrawalIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType) via DeriveAsDataRec PSpendRedeemer

validator :: Term s (PScriptContext :--> PUnit)
validator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo, pscriptContext'redeemer} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PSpendingScript ownRef _datum -> P.do
      PRedeemer redeemer <- pmatch pscriptContext'redeemer
      PSpendRedeemer
        { pspendRedeemer'inputIndex
        , pspendRedeemer'withdrawalRedeemerIndex
        , pspendRedeemer'withdrawalIndex
        } <-
        pmatch $ pfromData $ punsafeCoerce @(PAsData PSpendRedeemer) redeemer
      inputIndex <- plet $ pfromData pspendRedeemer'inputIndex
      PTxInfo {ptxInfo'inputs, ptxInfo'redeemers, ptxInfo'wdrl} <- pmatch pscriptContext'txInfo
      inputs <- plet $ pfromData ptxInfo'inputs

      -- Select the input using the redeemer's zero-based input index.
      -- Unlike pelemAt, dropList treats negative counts as zero.
      PTxInInfo {ptxInInfo'outRef = pickedOutRef, ptxInInfo'resolved = pickedOutput} <-
        pmatch $ pfromData $ pelemAtViaDrop inputIndex inputs

      passert
        "Selected input is not the own input"
        (pickedOutRef #== ownRef)

      PTxOut {ptxOut'address} <- pmatch pickedOutput
      PAddress {paddress'credential} <- pmatch ptxOut'address
      PScriptCredential scriptHashData <- pmatch paddress'credential

      WithdrawalWithAmount {withdrawalRedeemer, withdrawalAmount} <-
        validateWithdrawWithAmount @PTxOutRef
          (pfromData scriptHashData)
          (pfromData ptxInfo'redeemers)
          (pfromData pspendRedeemer'withdrawalRedeemerIndex)
          (pfromData ptxInfo'wdrl)
          (pfromData pspendRedeemer'withdrawalIndex)

      passert "Withdrawal must be coupled with the spent input" (withdrawalRedeemer #== pdata ownRef)
      passert "Withdrawal amount must be zero" (pto withdrawalAmount #== 0)

      pconstant ()
    PRewardingScript _stakeCred ->
      -- TODO: withdrawal validation
      perror
    _ -> perror
