module Plutarch.TxLevelMinter (
  spend,
  mint,
) where

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2 (PCurrencySymbol (..), PMintingPolicy, PScriptContext, PScriptPurpose (..), PTokenName)
import Plutarch.Api.V2.Contexts (PTxInfo)
import Plutarch.Builtin (pasInt)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Utils (pcountOfUniqueTokens, pheadSingleton, ptryLookupValue, (#>))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )

spend :: Term s (PTokenName :--> PData :--> PScriptContext :--> PUnit)
spend = phoistAcyclic $ plam $ \mintTN red ctx -> P.do
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  PSpending ownRef' <- pmatch ctxF.purpose
  ownRef <- plet $ pfield @"_0" # ownRef'
  txInfoF <- pletFields @'["inputs", "mint"] ctxF.txInfo
  let idx = pasInt # red
  indexedInput <- pletFields @'["outRef", "resolved"] (pelemAt @PBuiltinList # idx # txInfoF.inputs)
  ownInputF <- pletFields @'["value", "address"] indexedInput.resolved
  PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatch (pfield @"credential" # ownInputF.address)
  ownCurrencySymbol <- plet $ (pcon . PCurrencySymbol . pfromData . pto) ownValHash
  mintedValue <- plet (pnormalize # txInfoF.mint)
  tkPairs <- plet $ ptryLookupValue # pdata ownCurrencySymbol # mintedValue
  tkPair <- plet (pheadSingleton # tkPairs)
  let tnMinted = pfromData $ pfstBuiltin # tkPair
      numMinted = pfromData $ psndBuiltin # tkPair
  pif
    ( ptraceIfFalse "Incorrect indexed input" (ownRef #== indexedInput.outRef)
        #&& ptraceIfFalse "Too many assets" (pcountOfUniqueTokens # mintedValue #== 1)
        #&& ptraceIfFalse "Incorrect token name" (tnMinted #== mintTN)
        #&& ptraceIfFalse "Incorrect minted amount" ((numMinted #< 0) #|| (numMinted #> 0))
    )
    (pconstant ())
    perror

mint :: Term s (PData :--> PCurrencySymbol :--> PTxInfo :--> PUnit) -> Term s PMintingPolicy
mint mintLogic =
  plam $ \redeemer ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    PMinting ((pfield @"_0" #) -> cs') <- pmatchC ctxF.purpose
    cs <- pletC $ (pcon . PCurrencySymbol . pfromData . pto) cs'
    return $
      popaque $
        mintLogic # redeemer # cs # ctxF.txInfo
