{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.Vesting where

import PlutusTx 
    ( BuiltinData
    , compile
    )
import PlutusTx qualified

import PlutusTx.Prelude
    ( --traceError, 
      traceIfFalse
    --, otherwise
    --, (==)
    , Bool, (&&)
    --, Integer
    , ($)
    )

import PlutusTx.Builtins
    ( --mkI
    )

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

import Plutus.V2.Ledger.Contexts
        ( txSignedBy
        )

import Ledger
    ( contains
    )

import Pioneer.Util 
    ( Network (..)
    , mkUntypedScript
    , writeValidatorToFile
    , validatorAddressBech32
    , posixTimeFromIso8601
    , printDataToJSON
    )

import Prelude 
    ( IO
    , String
    )

import Data.Maybe (fromJust)


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
uscript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript script

validator :: Validator
validator = mkValidatorScript $$(compile [|| uscript ||])

save :: IO ()
save = writeValidatorToFile "./assets/vesting.plutus" validator


addressBech32 :: Network -> String
addressBech32 network = validatorAddressBech32 network validator

printVestingDatumJSON :: PubKeyHash -> String -> IO ()
printVestingDatumJSON pkh time = printDataToJSON $ VestingDatum
    { beneficiary = pkh
    , deadline    = fromJust $ posixTimeFromIso8601 time
    }
