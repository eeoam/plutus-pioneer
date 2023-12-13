{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week06.PatchedSwap where

import Pioneer.Plutus
import Pioneer.Util

data DatumSwap = DatumSwap
    { beneficiary :: PubKeyHash
    , price       :: Integer}
unstableMakeIsData ''DatumSwap

{-# INLINABLE script #-}
script :: DatumSwap -> () -> ScriptContext -> Bool
script ds _ ctx =
    traceIfFalse "Hey! You have to pay the owner!" outputToBeneficiary 
    &&
    traceIfFalse "Hey! You can only consume one script UTxO per Tx" consumeOnlyOneOutput where

    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    outputToBeneficiary :: Bool
    outputToBeneficiary = 
        valuePaidTo txInfo (beneficiary ds) == singleton adaSymbol adaToken (price ds)

    getScriptInputs :: [ TxInInfo ]
    getScriptInputs = filter isScriptInput allInputs
        where
            allInputs :: [ TxInInfo ]
            allInputs = txInfoInputs txInfo

            isScriptInput :: TxInInfo -> Bool
            isScriptInput i = case addressCredential . txOutAddress . txInInfoResolved $ i of
                ScriptCredential vh -> vh == ownHash ctx -- check that the validator required to spend this output is the current one
                _                   -> False

    consumeOnlyOneOutput :: Bool
    consumeOnlyOneOutput = case getScriptInputs of
        [ _ ] -> True
        _     -> False

{-# INLINABLE scriptu #-}
scriptu :: BuiltinData -> BuiltinData -> BuiltinData -> ()
scriptu = mkUntypedValidatorScript script

scriptc :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
scriptc = $$(compile [|| scriptu ||])

validator :: Validator
validator = mkValidatorScript scriptc