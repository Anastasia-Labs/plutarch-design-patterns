{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.ValidityRangeNormalization (
  NormalizedTimeRange (..),
  PNormalizedTimeRange (..),
  normalizeTimeRange,
) where

import Plutarch.Api.V1 (PPOSIXTimeRange)
import Plutarch.Prelude
import PlutusTx qualified

import Plutarch.Api.V2 (PExtended (..), PPOSIXTime (..))

data NormalizedTimeRange
  = ClosedRange Integer Integer
  | FromNegInf Integer
  | ToPosInf Integer
  | Always
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''NormalizedTimeRange
  [ ('ClosedRange, 0)
  , ('FromNegInf, 1)
  , ('ToPosInf, 2)
  , ('Always, 3)
  ]
PlutusTx.makeLift ''NormalizedTimeRange

data PNormalizedTimeRange (s :: S)
  = PClosedRange (Term s (PDataRecord '["lower" ':= PPOSIXTime, "upper" ':= PPOSIXTime]))
  | PFromNegInf (Term s (PDataRecord '["upper" ':= PPOSIXTime]))
  | PToPosInf (Term s (PDataRecord '["lower" ':= PPOSIXTime]))
  | PAlways (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PNormalizedTimeRange where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PNormalizedTimeRange

normalizeTimeRange :: Term s (PPOSIXTimeRange :--> PNormalizedTimeRange)
normalizeTimeRange = phoistAcyclic $ plam $ \timeRange ->
  pmatch (pfield @"_0" # (pfield @"from" # timeRange)) $ \case
    PFinite ((pfield @"_0" #) -> f) ->
      pmatch (pfield @"_0" # (pfield @"to" # timeRange)) $ \case
        PFinite ((pfield @"_0" #) -> t) ->
          pcon $
            PClosedRange $
              pdcons @"lower"
                # pdata f
                #$ pdcons @"upper"
                # pdata t
                #$ pdnil
        PPosInf _ ->
          pcon $
            PToPosInf $
              pdcons @"lower"
                # pdata f
                #$ pdnil
        _ -> perror
    PNegInf _ ->
      pmatch (pfield @"_0" # (pfield @"to" # timeRange)) $ \case
        PFinite ((pfield @"_0" #) -> t) ->
          pcon $
            PFromNegInf $
              pdcons @"upper"
                # pdata t
                #$ pdnil
        PPosInf _ -> pcon $ PAlways pdnil
        _ -> perror
    _ -> perror
