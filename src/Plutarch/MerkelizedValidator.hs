{-# LANGUAGE TemplateHaskell #-}

module Plutarch.MerkelizedValidator (
  spend,
  withdraw,
  WithdrawRedeemer (..),
  PWithdrawRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PCredential,
  PRedeemer (..),
  PScriptContext (..),
  PScriptInfo (..),
  PScriptPurpose (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusTx (BuiltinData)
import PlutusTx qualified

data WithdrawRedeemer = WithdrawRedeemer
  { inputState :: [BuiltinData]
  , outputState :: [BuiltinData]
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''WithdrawRedeemer [('WithdrawRedeemer, 0)]

data PWithdrawRedeemer (s :: S) = PWithdrawRedeemer
  { pwithdrawRedeemer'inputState ::
      Term s (PAsData (PBuiltinList PData))
  , pwithdrawRedeemer'outputState ::
      Term s (PAsData (PBuiltinList PData))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PWithdrawRedeemer

deriving via
  DeriveDataPLiftable PWithdrawRedeemer WithdrawRedeemer
  instance
    PLiftable PWithdrawRedeemer

spend ::
  Term s PCredential ->
  Term s (PBuiltinList PData) ->
  Term s (AssocMap.PUnsortedMap PScriptPurpose PRedeemer) ->
  Term s (PBuiltinList PData)
spend stakCred inputState redeemers = P.do
  PRedeemer redeemerData <- pmatch $ pfindRewardingRedeemer stakCred redeemers
  PWithdrawRedeemer
    { pwithdrawRedeemer'inputState
    , pwithdrawRedeemer'outputState
    } <-
    pmatch $ pfromData $ pparseData @PWithdrawRedeemer redeemerData
  pif
    (inputState #== pfromData pwithdrawRedeemer'inputState)
    (pfromData pwithdrawRedeemer'outputState)
    perror

withdraw ::
  Term s (PBuiltinList PData :--> PBuiltinList PData) ->
  Term s (PScriptContext :--> PUnit)
withdraw f =
  plam $ \ctx -> P.do
    PScriptContext {pscriptContext'redeemer, pscriptContext'scriptInfo} <- pmatch ctx
    PRewardingScript _ <- pmatch pscriptContext'scriptInfo
    PRedeemer redeemer <- pmatch pscriptContext'redeemer
    PWithdrawRedeemer
      { pwithdrawRedeemer'inputState
      , pwithdrawRedeemer'outputState
      } <-
      pmatch $ pfromData $ pparseData @PWithdrawRedeemer redeemer
    pif
      ( (f # pfromData pwithdrawRedeemer'inputState)
          #== pfromData pwithdrawRedeemer'outputState
      )
      (pconstant ())
      perror

pfindRewardingRedeemer ::
  Term s PCredential ->
  Term s (AssocMap.PUnsortedMap PScriptPurpose PRedeemer) ->
  Term s PRedeemer
pfindRewardingRedeemer stakingCredential redeemers =
  ( pfix $ \self -> plam $ \pairs ->
      pelimList
        ( \pair rest ->
            pmatch pair $ \(PBuiltinPair purposeData redeemerData) ->
              pif
                (pfromData purposeData #== pcon (PRewarding stakingCredential))
                (pfromData redeemerData)
                (self # rest)
        )
        perror
        pairs
  )
    # pto (pto redeemers)
