module Plutarch.TxLevelMinter (
  spend,
  mint,
) where

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
import Plutarch.Prelude hiding ((#>))
import Plutarch.Utils (
  PWrapperRedeemer (..),
  pcountOfUniqueTokens,
  pheadSingleton,
  ptryLookupValue,
  (#>),
 )

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
      mintedValue <- plet $ pfromData ptxInfo'mint
      tkPairs <- plet $ ptryLookupValue # pdata ownCurrencySymbol # mintedValue
      tkPair <- plet $ pheadSingleton # tkPairs
      PBuiltinPair tokenNameData amountData <- pmatch tkPair
      let tnMinted = pfromData tokenNameData
          numMinted = pfromData amountData
      pif
        ( ptraceInfoIfFalse "Incorrect indexed input" (ownRef #== ptxInInfo'outRef)
            #&& ptraceInfoIfFalse "Too many assets" (pcountOfUniqueTokens # mintedValue #== 1)
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
