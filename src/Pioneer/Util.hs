module Pioneer.Util where

import Data.ByteString.Short qualified as BSS
import Data.ByteString.Lazy qualified as BSL

import Codec.Serialise
    ( Serialise
    , serialise
    )

import Cardano.Api
    ( PlutusScript
    , PlutusScriptV2
    , writeFileTextEnvelope
    , Error (displayError)
    )

import Cardano.Api.Shelley
    ( PlutusScript (..)
    )

import PlutusTx
    ( BuiltinData
    , UnsafeFromData (..)
    )

import PlutusTx.Prelude
    ( traceError
    )

import Plutus.V2.Ledger.Api qualified as PlutusV2

writeValidatorToFile :: FilePath -> PlutusV2.Validator -> IO ()
writeValidatorToFile filePath = writeScriptToFile filePath . validatorToScript

writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
    writeFileTextEnvelope filePath Nothing script >>= \case
        Left err -> print $ displayError err
        Right () -> putStrLn $ "Serialized script to: " ++ filePath


validatorToScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
validatorToScript = serializableToScript

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- | A more efficient implementation of the `mkUntypedMintingPolicy` method of the `IsScriptContext` typeclass.
-- Copyright (c) 2023 Ian Burzynski
{-# INLINABLE mkUntypedScript #-}
mkUntypedScript :: ( UnsafeFromData a, UnsafeFromData b)
                   => (a -> b -> PlutusV2.ScriptContext -> Bool)
                   -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
mkUntypedScript f a b ctx =
    check $
        f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData ctx)

check b = if b then () else traceError "script validation failed"