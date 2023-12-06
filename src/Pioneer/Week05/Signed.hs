{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week05.Signed where

import Pioneer.Plutus

import Pioneer.Util
    ( mkUntypedMintingScript
    , writePolicyToFile
    , writeCodeToFile
    , currencySymbol
    )

import Prelude
    ( IO
    , Show (show)
    )

import Text.Printf
    ( printf
    )

{-# INLINABLE script #-}
script :: PubKeyHash -> () -> ScriptContext -> Bool
script pkh () ctx =
    traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh
    
{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedMintingScript . script . unsafeFromBuiltinData

cscript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
cscript = $$(compile [|| uscript ||])

policy :: PubKeyHash -> MintingPolicy
policy pkh = mkMintingPolicyScript $ cscript `applyCode` liftCode (toBuiltinData pkh)

save_code :: IO ()
save_code = writeCodeToFile "assets/signed.plutus" cscript

save_policy :: PubKeyHash -> IO ()
save_policy pkh = writePolicyToFile (printf "assets/signed-%s.plutus" $ show pkh) (policy pkh)

currency_symbol :: PubKeyHash -> CurrencySymbol
currency_symbol = currencySymbol . policy
