{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.Vesting where

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
    ( mkUntypedScript
    , writeValidatorToFile
    )

import Prelude (IO)


data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE script #-}
script :: VestingDatum -> () -> ScriptContext -> Bool
script dat () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [|| uscript ||])

save :: IO ()
save = writeValidatorToFile "./assets/vesting.plutus" validator


