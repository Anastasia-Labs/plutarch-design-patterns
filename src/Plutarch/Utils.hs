module Plutarch.Utils (
  ptryOwnInput,
) where

import Plutarch.Api.V2 (PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Prelude

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput =
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs
