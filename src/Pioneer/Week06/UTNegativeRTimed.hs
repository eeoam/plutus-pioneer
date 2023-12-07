{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week06.UTNegativeRTimed where

import Control.Monad
    ( mapM
    , replicateM
    , unless
    )

import Pioneer.Week06.NegativeRTimed qualified as OnChain

import Plutus.Model
    ( Ada (Lovelace)
    , DatumMode (HashDatum)
    , Run
    , Tx
    , ada
    , adaValue
    , defaultBabbage
    , mustFail
    , newUser
    , noErrors
    , sendValue
    , testNoErrors
    , valueAt
    , TypedValidator (TypedValidator)
    , UserSpend
    , currentTimeRad
    , logError
    , payToKey
    , payToScript
    , spend
    , spendScript
    , submitTx
    , toV2
    , userSpend
    , utxoAt
    , validateIn
    , waitUntil
    )

import Pioneer.Plutus
import Prelude
    ( IO
    , mconcat
    )

import Test.Tasty
    ( defaultMain
    , testGroup
    )

test :: IO ()
test = defaultMain $ do
    testGroup
        "Testing validator with some sensible values"
        [ good "User 1 locks and user 2 takes with R = -42 after deadline succeeds" $ testScript 50 (-42)
        , good "User 1 locks and user 2 takes with R = 0 after deadline succeeds" $ testScript 50 0
        , bad "User 1 locks and user 2 takes with R = 42 after deadline fails" $ testScript 50 42
        , bad "User 1 locks and user 2 takes with R = -42 before deadline fails" $ testScript 5000 (-42)
        , bad "User 1 locks and user 2 takes with R = 0 before deadline fails" $ testScript 5000 0
        , bad "User 1 locks and user 2 takes with R = 42 before deadline fails" $ testScript 5000 42
        ]
    where
        good = testNoErrors (adaValue 10_000_000) defaultBabbage
        bad msg = good msg . mustFail

waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000

setUpUsers :: Run [ PubKeyHash ]
setUpUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator

lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
    mconcat
        [ userSpend usp
        , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
        ]

consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
    mconcat
        [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
        , payToKey usr val
        ]

testScript :: POSIXTime -> Integer -> Run ()
testScript d r = do
    [ u1, u2 ] <- setUpUsers

    let val = adaValue 100
    sp <- spend u1 val
    submitTx u1 $ lockingTx d sp val

    waitUntil waitBeforeConsumingTx

    utxos <- utxoAt valScript
    let [(ref, out)] = utxos
    ct <- currentTimeRad 100
    tx <- validateIn ct $ consumingTx d r u2 ref (txOutValue out)
    submitTx u2 tx

    [ v1, v2 ] <- mapM valueAt [ u1, u2 ]
    unless (v1 == adaValue 900 && v2 == adaValue 1100) $
        logError "Final balances are incorrect"