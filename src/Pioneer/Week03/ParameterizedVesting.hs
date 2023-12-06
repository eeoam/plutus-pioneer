{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.ParameterizedVesting where

import PlutusTx 
    ( BuiltinData
    , compile
    , applyCode
    , liftCode
    )
import PlutusTx qualified




import Plutus.V2.Ledger.Api
    ( Validator
    , mkValidatorScript
    , ScriptContext(..)
    , TxInfo
    , POSIXTime
    , from
    --, to
    , txInfoValidRange
    , PubKeyHash
    )

import PlutusTx.Prelude
    ( --traceError, 
      traceIfFalse
    --, otherwise
    --, (==)
    --, 
    , Bool
    , (&&)
    --, Integer
    , ($)
    , (.)
    )

import PlutusTx.Builtins
    ( --mkI
    )

import Plutus.V2.Ledger.Contexts
        ( txSignedBy
        )

import Ledger
    ( contains
    )

import Pioneer.Util 
    ( --Network (..)
    {-,-} mkUntypedScript
    , writeValidatorToFile
    --, validatorAddressBech32
    --, posixTimeFromIso8601
    --, printDataToJSON
    )

import Prelude 
    ( IO
    --, String
    )

--import Data.Maybe (fromJust)


data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
PlutusTx.makeLift ''VestingParams

{-# INLINABLE script #-}
script :: VestingParams -> () -> () -> ScriptContext -> Bool
script params () () ctx =
    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary params

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript . script

validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| uscript ||]) `applyCode` liftCode params)

save :: VestingParams -> IO ()
save = writeValidatorToFile "./assets/parameterized-vesting.plutus" . validator


