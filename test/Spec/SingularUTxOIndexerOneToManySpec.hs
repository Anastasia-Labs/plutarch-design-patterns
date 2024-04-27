module Spec.SingularUTxOIndexerOneToManySpec (
  spend,
) where

import Plutarch.Api.V2 (PValidator)
import Plutarch.Prelude
import Plutarch.SingularUTxOIndexerOneToMany qualified as SingularUTxOIndexerOneToMany
import Spec.Utils qualified as Utils

spend :: Term s PValidator
spend = SingularUTxOIndexerOneToMany.spend Utils.inputValidator Utils.inputOutputValidator Utils.collectiveOutputValidator
