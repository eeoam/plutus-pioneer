{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week03.Homework2 where

import PlutusTx 
    ( BuiltinData
    , compile
    , applyCode
    , liftCode
    )


import PlutusTx.Prelude
    ( traceIfFalse
    --, traceError
    --, otherwise
    --, (==)
    , Bool, (&&)
    --, Integer
    , ($)
    , (.)
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
    ( mkUntypedScript
    --, writeValidatorToFile
    )

--import Prelude (IO)



{-# INLINABLE script #-}
-- | This should validate if 
-- | the transaction has a signature from the parameterand beneficiary and
-- | the deadline has passed.
script :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
script beneficiary deadline () ctx =
    traceIfFalse "beneficiary signature missing" signedByBeneficiary
    &&
    traceIfFalse "deadline not reached" deadlinePassed where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info beneficiary

    deadlinePassed :: Bool
    deadlinePassed = from deadline `contains` txInfoValidRange info

{-# INLINABLE uscript #-}
uscript :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
uscript = mkUntypedScript . script

validator :: PubKeyHash -> Validator
validator pkh = mkValidatorScript $
                $$(compile [|| uscript ||]) `applyCode` liftCode pkh