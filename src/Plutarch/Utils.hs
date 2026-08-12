{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Utils (
  ptryOwnInput,
  pheadSingleton,
  passert,
  pcountOfUniqueTokens,
  ptryLookupValue,
  (#>),
  preverse,
  WrapperRedeemer (..),
  PWrapperRedeemer (..),
) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PTokenName,
  PTxInInfo (..),
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Prelude hiding ((#>))
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

ptryOwnInput ::
  (PIsListLike list (PAsData PTxInInfo)) =>
  Term s (list (PAsData PTxInInfo) :--> PTxOutRef :--> PTxOut)
ptryOwnInput =
  plam $ \inputs ownRef ->
    precList
      ( \self inputData rest ->
          pmatch (pfromData inputData) $ \PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} ->
            pif
              (ownRef #== ptxInInfo'outRef)
              ptxInInfo'resolved
              (self # rest)
      )
      (const perror)
      # inputs

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton =
  phoistAcyclic $
    plam $ \xs ->
      pelimList
        (\x rest -> pif (pnull # rest) x (ptraceInfoError "List contains more than one element."))
        perror
        xs

passert :: T.Text -> Term s PBool -> Term s a -> Term s a
passert longErrorMsg b inp = pif b inp $ ptraceInfoError (pconstant longErrorMsg)

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens :: Term s (PMintValue :--> PInteger)
pcountOfUniqueTokens =
  phoistAcyclic $
    plam $ \value ->
      pfoldl
        # plam
          ( \count pair ->
              pmatch pair $ \(PBuiltinPair _ tokenMapData) ->
                count + plength # ptokenPairs (pfromData tokenMapData)
          )
        # 0
        # pmintValuePairs value

ptryLookupValue ::
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PMintValue
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptryLookupValue =
  phoistAcyclic $
    plam $ \policyId value ->
      ( pfix $ \self -> plam $ \pairs ->
          pelimList
            ( \pair rest ->
                pmatch pair $ \(PBuiltinPair currencySymbol tokenMapData) ->
                  pif
                    (currencySymbol #== policyId)
                    (ptokenPairs $ pfromData tokenMapData)
                    (self # rest)
            )
            perror
            pairs
      )
        # pmintValuePairs value

(#>) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

preverse :: (PIsListLike l a) => Term s (l a :--> l a)
preverse =
  phoistAcyclic $
    pfoldl # plam (\ys y -> pcons # y # ys) # pnil

pmintValuePairs ::
  Term s PMintValue ->
  Term
    s
    ( PBuiltinList
        ( PBuiltinPair
            (PAsData PCurrencySymbol)
            (PAsData (AssocMap.PSortedMap PTokenName PInteger))
        )
    )
pmintValuePairs value = pto (pto (pto (pto value)))

ptokenPairs ::
  Term s (AssocMap.PSortedMap PTokenName PInteger) ->
  Term
    s
    (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
ptokenPairs tokenMap = pto (pto tokenMap)
