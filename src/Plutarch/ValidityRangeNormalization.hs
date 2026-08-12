{-# LANGUAGE TemplateHaskell #-}

module Plutarch.ValidityRangeNormalization (
  NormalizedTimeRange (..),
  PNormalizedTimeRange (..),
  normalizeTimeRange,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PExtended (..),
  PInterval (..),
  PLowerBound (..),
  PPosixTime (..),
  PUpperBound (..),
 )
import Plutarch.Prelude
import PlutusTx qualified

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

data PNormalizedTimeRange (s :: S)
  = PClosedRange (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PFromNegInf (Term s (PAsData PInteger))
  | PToPosInf (Term s (PAsData PInteger))
  | PAlways
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving (PlutusType, PValidateData) via DeriveAsDataStruct PNormalizedTimeRange

deriving via
  DeriveDataPLiftable PNormalizedTimeRange NormalizedTimeRange
  instance
    PLiftable PNormalizedTimeRange

normalizeTimeRange :: Term s (PInterval PPosixTime :--> PNormalizedTimeRange)
normalizeTimeRange =
  phoistAcyclic $
    plam $ \timeRange ->
      pmatch timeRange $ \PInterval {pinterval'from, pinterval'to} ->
        pmatch pinterval'from $ \(PLowerBound lower _) ->
          pmatch pinterval'to $ \(PUpperBound upper _) ->
            pmatch lower $ \case
              PFinite lowerData ->
                pmatch (pfromData lowerData) $ \(PPosixTime lowerBound) ->
                  pmatch upper $ \case
                    PFinite upperData ->
                      pmatch (pfromData upperData) $ \(PPosixTime upperBound) ->
                        pcon $ PClosedRange (pdata lowerBound) (pdata upperBound)
                    PPosInf -> pcon $ PToPosInf (pdata lowerBound)
                    _ -> perror
              PNegInf ->
                pmatch upper $ \case
                  PFinite upperData ->
                    pmatch (pfromData upperData) $ \(PPosixTime upperBound) ->
                      pcon $ PFromNegInf (pdata upperBound)
                  PPosInf -> pcon PAlways
                  _ -> perror
              _ -> perror
