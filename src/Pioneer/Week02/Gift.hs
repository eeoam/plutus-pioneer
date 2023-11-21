module Pioneer.Week02.Gift where

import PlutusTx 
    ( BuiltinData
    , compile
    )

import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    )

import Pioneer.Util

-- | This validator always succeeds.
{-# INLINABLE script #-}
script :: BuiltinData -> BuiltinData -> BuiltinData -> ()
script _ _ _ = ()

scriptCompiled :: Validator
scriptCompiled = mkValidatorScript $$(compile [||script||])

saveVal :: IO ()
saveVal = writeValidatorToFile "./assets/gift.plutus" scriptCompiled



