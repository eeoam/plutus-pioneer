{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week02.FortyTwo where

import PlutusTx 
    ( BuiltinData
    , compile
    )

import PlutusTx.Prelude
    ( traceError
    , otherwise
    , (==)
    )

import PlutusTx.Builtins
    ( mkI
    )

import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    )

import Pioneer.Util

import Prelude (IO)

-- | This validator succeds only if the redeemer is 42.
{-# INLINABLE script #-}
script :: BuiltinData -> BuiltinData -> BuiltinData -> ()
script _ r _
    | r == mkI 42 = ()
    | otherwise            = traceError "expected 42"

validator :: Validator
validator = mkValidatorScript $$(compile [||script||])

save :: IO ()
save = writeValidatorToFile "./assets/fortytwo.plutus" validator