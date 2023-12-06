module Pioneer.Week05.Free where

import Pioneer.Plutus

import Pioneer.Util
    ( mkUntypedMintingScript
    , writePolicyToFile
    , currencySymbol
    )

{-# INLINABLE script #-}
script :: () -> ScriptContext -> Bool
script () _ = True
    
{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> ()
uscript = mkUntypedMintingScript script

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(compile [|| uscript ||])

save :: IO ()
save = writePolicyToFile "assets/free.plutus" policy

currency_symbol :: CurrencySymbol
currency_symbol = currencySymbol policy
