{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week05.Homework1 where

import Pioneer.Plutus

import Pioneer.Util
    ( mkUntypedMintingScript
    )

-- | This policy should only allow minting (or burning) of tokens if
-- | the owner the specified PubKeyHash has signed the transaction and if
-- | the specified deadline has not passed.
{-# INLINABLE script #-}
script :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
script pkh deadline () ctx =
    traceIfFalse "missing signature" signed
    &&
    traceIfFalse "deadline passed" beforeDeadline
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signed :: Bool
        signed = txSignedBy info pkh

        beforeDeadline :: Bool
        beforeDeadline = to deadline `contains` txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
uscript pkh deadline = mkUntypedMintingScript $ script pkh deadline

cscript :: CompiledCode (PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ())
cscript = $$(compile [|| uscript ||])

policy :: PubKeyHash -> POSIXTime -> MintingPolicy
policy pkh deadline = mkMintingPolicyScript $ cscript 
                    `applyCode` liftCode pkh
                    `applyCode` liftCode deadline