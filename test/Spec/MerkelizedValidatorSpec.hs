module Spec.MerkelizedValidatorSpec (
  psumOfSquares,
  unitTest,
) where

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V2 (
  PScriptHash (..),
  PStakingCredential (..),
  PValidator,
 )
import Plutarch.Builtin (pasInt)
import Plutarch.MerkelizedValidator (spend)
import Plutarch.Num ((#*), (#+))
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure), testEvalCase, tryFromPTerm)
import Plutarch.Utils (pheadSingleton)
import PlutusTx qualified
import Test.Tasty (TestTree)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
 )

psumOfSquares :: (PIsListLike list PInteger) => Term s (list PInteger :--> list PInteger)
psumOfSquares =
  plam $ \xs ->
    let result = pfoldl # plam (\x y -> (x #* x) #+ y) # 0 # xs
     in psingleton # result

validator :: Term s PStakingCredential -> Term s PValidator
validator stakeCred =
  plam $ \x y ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo"] ctx
    txInfoF <- pletFieldsC @'["redeemers"] ctxF.txInfo
    sum' <- pletC $ pheadSingleton #$ spend stakeCred (pcons # x # (pcons # y # pnil)) txInfoF.redeemers
    sum <- pletC $ pasInt # sum'
    return $
      pif
        (sum #< 42)
        (popaque $ pconstant ())
        perror

scByteString :: Term s PByteString
scByteString = pconstant "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"

stakeCred :: Term s PStakingCredential
stakeCred =
  pcon $ PStakingHash $ pdcons @"_0" # (pdata $ pcon $ PScriptCredential $ pdcons @"_0" # (pdata . pcon . PScriptHash) scByteString # pdnil) # pdnil

-- TODO(hadelive)
unitTest :: TestTree
unitTest = tryFromPTerm "Merkelized Validator Unit Test" (validator stakeCred) $ do
  testEvalCase
    "Fail"
    Failure
    [ PlutusTx.toData ()
    , PlutusTx.toData ()
    , PlutusTx.toData ()
    ]
