module Pioneer.Util 
    ( Network (..)
    , mkUntypedScript
    , writeValidatorToFile
    , validatorAddressBech32
    , posixTimeFromIso8601
    , printDataToJSON
    ) where

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

import Cardano.Ledger.BaseTypes
    (
      Network (..)
    )

import Data.Text           qualified as Text
import Cardano.Api         qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Credential 
    ( StakeReference (..) {- StakeRefNull -}
    , Credential (..) {- ScriptHashObj -}
    )

import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format.ISO8601 qualified as Time

import Data.ByteString.Char8 qualified as BS8
import Data.Aeson (Value)
import Plutus.V1.Ledger.Api (ToData)
import Cardano.Api 
    ( prettyPrintJSON
    )
import Cardano.Api.Shelley
    ( scriptDataToJsonDetailedSchema
    , fromPlutusData
    )

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



validatorAddressBech32 :: Network -> PlutusV2.Validator -> String
validatorAddressBech32 network v 
    = Text.unpack
    $ Api.serialiseToBech32
    $ Api.ShelleyAddress
          network
          (ScriptHashObj $ Api.toShelleyScriptHash $ validator_hash v)
          StakeRefNull

validator_hash :: PlutusV2.Validator -> Api.ScriptHash
validator_hash = hashScript .  validatorToScript


hashScript :: PlutusScript PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

posixTimeFromIso8601 :: String -> Maybe PlutusV2.POSIXTime
posixTimeFromIso8601 s = do
    t <- Time.formatParseM Time.iso8601Format s
    let seconds = Time.utcTimeToPOSIXSeconds t
    let milliSeconds = round $ 1000 * seconds :: Integer
    return $ fromInteger milliSeconds

printDataToJSON :: ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

dataToJSON :: ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . fromPlutusData . PlutusV2.toData