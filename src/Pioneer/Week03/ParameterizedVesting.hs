{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.ParameterizedVesting where

import PlutusTx 
    ( BuiltinData
    , compile
    , applyCode
    , liftCode
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


data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
PlutusTx.makeLift ''VestingDatum

{-# INLINABLE script #-}
script :: VestingParams -> () -> () -> ScriptContext -> Bool
script params () () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> Bool
uscript = mkUntypedScript . script

validator :: Validator
validator params = mkValidatorScript ($$(compile [|| uscript ||]) `applyCode` liftCode params)

save :: VestingParams -> IO ()
save = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator


