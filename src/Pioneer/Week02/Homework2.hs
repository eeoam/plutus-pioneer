{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week02.Homework2 where

import PlutusTx 
    ( BuiltinData
    , compile
    )
import PlutusTx qualified

import PlutusTx.Prelude
    ( 
    --  traceError, 
    --  traceIfFalse
    --, otherwise
    --, (==), 
      (/=)
    , Bool
    --, (&&)
    --, Integer
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

--import Prelude (IO)

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }
PlutusTx.unstableMakeIsData ''MyRedeemer


{-# INLINABLE script #-}
-- | A validator that unlocks the funds if My Redeemer's flags are different
script :: () -> MyRedeemer -> ScriptContext -> Bool
script () myRedeemer _ = flag1 myRedeemer /= flag2 myRedeemer

uscript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile[||uscript||])