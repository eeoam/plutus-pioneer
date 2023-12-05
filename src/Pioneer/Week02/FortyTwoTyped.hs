{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week02.FortyTwoTyped where

import PlutusTx 
    ( BuiltinData
    , compile
    )

import PlutusTx.Prelude
    ( --traceError, 
      traceIfFalse
    --, otherwise
    , (==)
    , Bool
    , Integer
    , ($)
    )

import PlutusTx.Builtins
    ( --mkI
    )

import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    , ScriptContext(..)
    )

import Pioneer.Util

import Prelude (IO)

-- | This validator succeeds only if the redeemer is 42.
{-# INLINABLE script #-}
script :: () -> Integer -> ScriptContext -> Bool
script _ r _ = traceIfFalse "expected 42" $ r == 42

{-# INLINABLE script_untyped #-}
script_untyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
script_untyped = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [||script_untyped||])

save :: IO ()
save = writeValidatorToFile "./assets/fortytwotyped.plutus" validator

