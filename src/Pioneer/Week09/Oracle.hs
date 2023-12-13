{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week09.Oracle where

import Pioneer.Plutus
import Pioneer.Util

import Prelude 
    ( Show (show)
    , span
    , IO
    , undefined
    )
import Prelude qualified ((/=))
import Data.String 
    ( IsString (fromString)
    , String
    )
import Text.Printf ( printf )

{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe Integer
parseOracleDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash dh -> do
                     Datum d <- findDatum dh info
                     fromBuiltinData d

data OracleParams = OracleParams
    { oNFT      :: AssetClass
    , oOperator :: PubKeyHash
    }
makeLift ''OracleParams

data OracleRedeemer = Update | Delete
    deriving Prelude.Show
unstableMakeIsData ''OracleRedeemer

-- Oracle Datum
type Rate = Integer

{-# INLINABLE script #-}
script :: OracleParams -> Rate -> OracleRedeemer -> ScriptContext -> Bool
script oracle _ r ctx =
    case r of
        Update -> traceIfFalse "token missing from input" inputHasToken &&
                  traceIfFalse "token missing from output" outputHasToken &&
                  traceIfFalse "operator signature missing" checkOperatorSignature &&
                  traceIfFalse "invalid output datum" validOutputDatum
        Delete -> traceIfFalse "operator signature missing" checkOperatorSignature
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkOperatorSignature :: Bool
        checkOperatorSignature = txSignedBy info $ oOperator oracle

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "oracle input missing"
            Just i  -> txInInfoResolved i

        inputHasToken :: Bool 
        inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT oracle) == 1

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [ o ] -> o
            _     -> traceError "expected exactly one oracle output"

        outputHasToken :: Bool
        outputHasToken = assetClassValueOf (txOutValue ownOutput) (oNFT oracle) == 1

        validOutputDatum :: Bool
        validOutputDatum = isJust $ parseOracleDatum ownOutput info
    
{-# INLINABLE scriptu #-}
scriptu :: OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
scriptu = mkUntypedValidatorScript . script

scriptc :: CompiledCode (OracleParams -> BuiltinData -> BuiltinData -> BuiltinData -> ())
scriptc = $$(compile [|| scriptu ||])

validator :: OracleParams -> Validator
validator oracle = mkValidatorScript $  scriptc `applyCode` liftCode oracle

{-# INLINABLE scriptuLucid #-}
scriptuLucid :: BuiltinData -> BuiltinData -> BuiltinData 
             -> BuiltinData -> BuiltinData -> BuiltinData -> ()
scriptuLucid cs tn pkh = mkUntypedValidatorScript $ script op
    where
        op = OracleParams
            { oNFT = AssetClass (unsafeFromBuiltinData cs, unsafeFromBuiltinData tn)
            , oOperator = unsafeFromBuiltinData pkh
            }

validatorCode :: CompiledCode 
                    (  BuiltinData -> BuiltinData -> BuiltinData 
                    -> BuiltinData -> BuiltinData -> BuiltinData -> ()
                    )
validatorCode = $$(compile [|| scriptuLucid ||])

saveOracleCode :: IO ()
saveOracleCode = writeCodeToFile "assets/oracle.plutus" validatorCode

saveOracleScript :: String -> PubKeyHash -> IO ()
saveOracleScript symbol pkh = do
    let 
    writeValidatorToFile fp $ validator op
    where
        op = OracleParams
             { oNFT = parseToken symbol
             , oOperator = pkh
             }
        fp = printf "assets/oracle-%s-%s.plutus" (take 3 (show pkh)) $ take 3 (show pkh)

parseToken :: String -> AssetClass
parseToken s = 
    let
        (x,y) = span (Prelude./= '.') s
    in
        AssetClass (fromString x, fromString $ tail y)