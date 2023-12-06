{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week05.NFT where

import Pioneer.Plutus

import Pioneer.Util
    ( mkUntypedMintingScript
    , writePolicyToFile
    , writeCodeToFile
    , currencySymbol
    , bytesToHex
    )

import Text.Printf
    ( printf
    )

import Prelude
    ( IO
    , String
    , Show (show)
    )

import Data.ByteString.Char8 qualified as BS8

{-# INLINABLE script #-}
script :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
script oref tn () ctx =
    traceIfFalse "UTxO not consumed" hasUTxO
    &&
    traceIfFalse "wrong amount minted" checkMintedAmount 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(_, tn'', amt)] -> tn'' == tn && amt == 1
            _                -> False


{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript tid ix tn' = mkUntypedMintingScript $ script oref tn
    where
        oref :: TxOutRef
        oref = TxOutRef (TxId $ unsafeFromBuiltinData tid) (unsafeFromBuiltinData ix)

        tn :: TokenName
        tn = unsafeFromBuiltinData tn'

cscript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
cscript = $$(compile [|| uscript ||])

policy :: TxOutRef -> TokenName -> MintingPolicy
policy oref tn = mkMintingPolicyScript $ cscript 
                    `applyCode` liftCode (toBuiltinData $ getTxId $ txOutRefId oref)
                    `applyCode` liftCode (toBuiltinData $ txOutRefIdx oref)
                    `applyCode` liftCode (toBuiltinData tn)

save_code :: IO ()
save_code = writeCodeToFile "assets/nft.plutus" cscript

save_policy :: TxOutRef -> TokenName -> IO ()
save_policy oref tn = writePolicyToFile filePath (policy oref tn)
    where
        filePath = printf "assets/nft-%s#%d-%s.plutus" (show $ txOutRefId oref) (txOutRefIdx oref) tn'

        tn' :: String
        tn' = case unTokenName tn of
            (BuiltinByteString bytes) -> BS8.unpack $ bytesToHex bytes

currency_symbol :: TxOutRef -> TokenName -> CurrencySymbol
currency_symbol oref = currencySymbol . policy oref
