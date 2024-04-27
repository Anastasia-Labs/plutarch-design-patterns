module Spec.Utils (
  inputValidator,
  inputOutputValidator,
  collectiveOutputValidator,
) where

import Plutarch.Api.V2 (PTxInInfo, PTxOut)
import Plutarch.Prelude

inputValidator :: Term s (PTxInInfo :--> PBool)
inputValidator = phoistAcyclic $
  plam $
    \_ -> pcon PTrue

inputOutputValidator :: Term s (PTxOut :--> PTxOut :--> PBool)
inputOutputValidator = phoistAcyclic $
  plam $
    \_ _ -> pcon PTrue

collectiveOutputValidator :: Term s (PBuiltinList PTxOut :--> PInteger :--> PBool)
collectiveOutputValidator = phoistAcyclic $
  plam $
    \_ _ -> pcon PTrue
