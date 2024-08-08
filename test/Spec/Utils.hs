module Spec.Utils (
  inputOutputValidator,
  inputValidator,
  collectiveOutputValidator,
  genByteString,
  mkAddressFromByteString,
  mkStakingHashFromByteString,
) where

import Plutarch.Api.V2 (PTxInInfo, PTxOut)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (Address (..), BuiltinByteString, Credential (..), ScriptHash (..), StakingCredential (..))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

import Test.Tasty.QuickCheck (Gen, elements, vectorOf)

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

genByteString :: Int -> Gen BuiltinByteString
genByteString n = do
  member <- vectorOf (n * 2) $ elements (['a' .. 'f'] ++ ['0' .. '9'])
  return (stringToBuiltinByteString member)

mkAddressFromByteString :: BuiltinByteString -> Address
mkAddressFromByteString = (flip Address) Nothing . ScriptCredential . ScriptHash

mkStakingHashFromByteString :: BuiltinByteString -> StakingCredential
mkStakingHashFromByteString = StakingHash . ScriptCredential . ScriptHash
