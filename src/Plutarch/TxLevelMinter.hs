{-# LANGUAGE TemplateHaskell #-}

module Plutarch.TxLevelMinter (
  spend,
  mint,
  WrapperRedeemer (..),
  PWrapperRedeemer (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.List (pheadSingleton)
import Plutarch.Core.Value qualified as Value
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol (..),
  PRedeemer (..),
  PScriptContext (..),
  PScriptHash (..),
  PScriptInfo (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusTx qualified

data WrapperRedeemer
  = None
  | WrapperRedeemer Integer
  deriving stock (Generic)

PlutusTx.makeIsDataIndexed ''WrapperRedeemer [('None, 0), ('WrapperRedeemer, 1)]

data PWrapperRedeemer (s :: S)
  = PNone
  | PWrapperRedeemer (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PWrapperRedeemer

deriving via
  DeriveDataPLiftable PWrapperRedeemer WrapperRedeemer
  instance
    PLiftable PWrapperRedeemer

spend :: Term s (PTokenName :--> PScriptContext :--> PUnit)
spend =
  phoistAcyclic $
    plam $ \mintTN ctx -> P.do
      PScriptContext
        { pscriptContext'txInfo
        , pscriptContext'redeemer
        , pscriptContext'scriptInfo
        } <-
        pmatch ctx
      PRedeemer redeemer <- pmatch pscriptContext'redeemer
      PWrapperRedeemer idxData <-
        pmatch $ pfromData $ pparseData @PWrapperRedeemer redeemer
      PSpendingScript ownRef _ <- pmatch pscriptContext'scriptInfo
      PTxInfo {ptxInfo'inputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
      indexedInput <-
        plet $
          pfromData $
            pelemAt
              # pfromData idxData
              # pfromData ptxInfo'inputs
      PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch indexedInput
      PTxOut {ptxOut'address} <- pmatch ptxInInfo'resolved
      PAddress {paddress'credential} <- pmatch ptxOut'address
      PScriptCredential ownValHashData <- pmatch paddress'credential
      PScriptHash ownValHash <- pmatch $ pfromData ownValHashData
      ownCurrencySymbol <- plet $ pcon $ PCurrencySymbol ownValHash
      mintedValue <- plet $ pto $ pfromData ptxInfo'mint
      tkPairs <- plet $ Value.ptryLookupValue # pdata ownCurrencySymbol # mintedValue
      tkPair <- plet $ pheadSingleton # tkPairs
      PBuiltinPair tokenNameData amountData <- pmatch tkPair
      let tnMinted = pfromData tokenNameData
          numMinted = pfromData amountData
      pif
        ( ptraceInfoIfFalse "Incorrect indexed input" (ownRef #== ptxInInfo'outRef)
            #&& ptraceInfoIfFalse "Too many assets" (Value.pcountOfUniqueTokens # mintedValue #== 1)
            #&& ptraceInfoIfFalse "Incorrect token name" (tnMinted #== mintTN)
            #&& ptraceInfoIfFalse "Incorrect minted amount" ((numMinted #< 0) #|| (numMinted #> 0))
        )
        (pconstant ())
        perror

mint ::
  Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit) ->
  Term s (PScriptContext :--> PUnit)
mint mintLogic =
  plam $ \ctx -> P.do
    PScriptContext
      { pscriptContext'txInfo
      , pscriptContext'redeemer
      , pscriptContext'scriptInfo
      } <-
      pmatch ctx
    PRedeemer redeemer <- pmatch pscriptContext'redeemer
    PMintingScript currencySymbolData <- pmatch pscriptContext'scriptInfo
    mintLogic
      # redeemer
      # pfromData currencySymbolData
      # pscriptContext'txInfo
