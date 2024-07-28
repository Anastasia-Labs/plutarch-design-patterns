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
import Plutarch.Api.V1 (AmountGuarantees (..), KeyGuarantees)
import Plutarch.Api.V2 (PCurrencySymbol, PMap (PMap), PTokenName, PTxInInfo, PTxOut, PTxOutRef, PValue (..))
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Prelude
import PlutusTx qualified

data WrapperRedeemer
  = None
  | WrapperRedeemer Integer

PlutusTx.makeLift ''WrapperRedeemer
PlutusTx.makeIsDataIndexed ''WrapperRedeemer [('None, 0), ('WrapperRedeemer, 1)]

data PWrapperRedeemer (s :: S)
  = PNone (Term s (PDataRecord '[]))
  | PWrapperRedeemer (Term s (PDataRecord '["idx" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PWrapperRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PWrapperRedeemer where type PLifted PWrapperRedeemer = WrapperRedeemer
deriving via (DerivePConstantViaData WrapperRedeemer PWrapperRedeemer) instance PConstantDecl WrapperRedeemer

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput =
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ptrace "." (ownRef #== txInFields.outRef)) txInFields.resolved (self # xs)) (const perror) # inputs

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> pif (pnull # xs) x (ptraceError "List contains more than one element.")) perror xs

passert ::
  forall (s :: S) (a :: PType).
  T.Text -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passert longErrorMsg b inp = pif b inp $ ptraceError (pconstant longErrorMsg)

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

ptryLookupValue ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptryLookupValue = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  tokens
              )
              (self # xs)
        )
        (const perror)
        # pto val'

(#>) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

preverse :: (PIsListLike l a) => Term s (l a :--> l a)
preverse =
  phoistAcyclic $
    pfoldl # plam (\ys y -> pcons # y # ys) # pnil
