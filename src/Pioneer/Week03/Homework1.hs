{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.Homework1 where

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
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE script #-}
-- | This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- | or if the beneficiary2 has signed the transaction and the deadline has passed.
script :: VestingDatum -> () -> ScriptContext -> Bool
script dat () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 = txSignedBy info $ beneficiary1 dat

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info $ beneficiary2 dat

    beforeDeadline :: Bool
    beforeDeadline = contains (to $ deadline dat) $ txInfoValidRange info

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> Bool
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [|| uscript ||])

save :: IO ()
save = writeValidatorToFile "./assets/vesting.plutus" validator


