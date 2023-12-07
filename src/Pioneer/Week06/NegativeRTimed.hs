{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week06.NegativeRTimed where

import Pioneer.Plutus
import Pioneer.Util

newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

{-# INLINABLE script #-}
script :: CustomDatum -> Integer -> ScriptContext -> Bool
script (MkCustomDatum d) r ctx =
    traceIfFalse "expected a negative redeemer" $ r <= 0
    &&
    traceIfFalse "deadline not reached" deadlineReached where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = from d `contains` txInfoValidRange info

{-# INLINABLE scripu #-}
scripu :: BuiltinData -> BuiltinData -> BuiltinData -> ()
scripu = mkUntypedValidatorScript script

scripc :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
scripc = $$(compile [|| scripu ||])

validator :: Validator
validator = mkValidatorScript scripc