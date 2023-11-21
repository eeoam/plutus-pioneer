module Pioneer.Week02.Burn where

import PlutusTx 
    ( BuiltinData
    , compile
    )

import PlutusTx.Prelude
    ( traceError
    )

import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    )

import Pioneer.Util

-- | This validator always fails.
{-# INLINABLE script #-}
script :: BuiltinData -> BuiltinData -> BuiltinData -> ()
script _ _ _ = traceError "it burns!!!"

validator :: Validator
validator = mkValidatorScript $$(compile [||script||])

save :: IO ()
save = writeValidatorToFile "./assets/burn.plutus" validator