{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week05.Homework2 where

import Pioneer.Plutus

import Pioneer.Util
    ( mkUntypedMintingScript
    )

-- | Minting policy for an NFT, where
-- | the minting transaction must consume the given UTxO as input and
-- | where the TokenName will be the empty ByteString.
{-# INLINABLE script #-}
script :: TxOutRef -> () -> ScriptContext -> Bool
script oref () ctx =
    traceIfFalse "UTxO not consumed" hasUTxO
    &&
    traceIfFalse "wrong token name" checkTokenName 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

        checkTokenName :: Bool
        checkTokenName = case flattenValue (txInfoMint info) of
            [(_, tn, _)] -> tn == TokenName ""
            _                -> False

{-# INLINABLE uscript #-}
uscript :: TxOutRef -> BuiltinData -> BuiltinData -> ()
uscript oref = mkUntypedMintingScript $ script oref

cscript :: CompiledCode (TxOutRef -> BuiltinData -> BuiltinData -> ())
cscript = $$(compile [|| uscript ||])

policy :: TxOutRef -> MintingPolicy
policy oref  = mkMintingPolicyScript $ cscript `applyCode` liftCode oref
