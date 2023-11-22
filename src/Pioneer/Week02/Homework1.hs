{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week02.Homework1 where

import PlutusTx 
    ( BuiltinData
    , compile
    )
import PlutusTx qualified

import PlutusTx.Prelude
    ( traceError, traceIfFalse
    , otherwise
    , (==)
    , Bool, (&&)
    , Integer
    , ($)
    )

import PlutusTx.Builtins
    ( mkI
    )

import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    , ScriptContext(..)
    )

import Pioneer.Util

import Prelude (IO)

{-# INLINABLE script #-}
-- | This should validate if and only if the two booleans in the redeemer are True!
script :: () -> (Bool, Bool) -> ScriptContext -> Bool
script _ (a,b) _ = a && b

uscript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [|| uscript ||])