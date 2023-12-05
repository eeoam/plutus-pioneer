{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week02.CustomTypes where

import PlutusTx 
    ( BuiltinData
    , compile
    )
import PlutusTx qualified

import PlutusTx.Prelude
    ( --traceError, 
      traceIfFalse
    --, otherwise
    , (==)
    , Bool
    , Integer
    --, ($)
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

newtype MyRedeemer = MyRedeemer Integer
PlutusTx.unstableMakeIsData ''MyRedeemer -- Use TH to create an IsData instance for MyRedeemer.

-- | This validator suceeds only if the redeemer is `MyRedeemer 42`.
{-# INLINABLE script #-}
script :: () -> MyRedeemer -> ScriptContext -> Bool
script _ (MyRedeemer r) _ = traceIfFalse "expected 42" (r == 42)

{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [||uscript||])

save :: IO ()
save = writeValidatorToFile "./assets/customtypes.plutus" validator