module Spec.SingularUTxOIndexerSpec (
  spend,
) where

import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude
import Plutarch.SingularUTxOIndexer qualified as SingularUTxOIndexer
import Spec.Utils qualified as Utils

spend :: Term s PValidator
spend = SingularUTxOIndexer.spend Utils.inputOutputValidator
