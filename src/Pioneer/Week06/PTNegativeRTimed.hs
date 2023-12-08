{-# LANGUAGE NoImplicitPrelude #-}

module Pioneer.Week06.PTNegativeRTimed where

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
    , initMock
    , runMock
    )

import Pioneer.Plutus
import Prelude
    ( IO
    , mconcat
    , Ord ((<), (>))
    , undefined
    )

import Test.Tasty
    ( defaultMain
    , testGroup
    )

import Test.Tasty.QuickCheck as QC
    ( testProperty
    )

import Test.QuickCheck
    ( Property
    , Testable (property)
    , Arbitrary (arbitrary)
    , choose
    , (==>)
    , collect
    )

import Test.QuickCheck.Monadic
    ( assert
    , monadic
    , run)


instance Testable a => Testable (Run a) where
    property rp = 
        let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000)
        in property a

instance Arbitrary POSIXTime where
    arbitrary = do
        n <- choose (0, 2000)
        return (POSIXTime n)

test :: IO ()
test = defaultMain $ do
    testGroup
        "Testing script properties"
        [ testProperty "Anything before the deadline always fails" prop_Before_Fails
        , testProperty "Positive redeemer after deadline always fails" prop_PositiveAfter_Fails
        , testProperty "Negative redeemer after deadline always succeeds" prop_NegativeAfter_Succeeds
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

prop_Before_Fails :: POSIXTime -> Integer -> Property
prop_Before_Fails d r = (d > 1001) ==> runChecks False d r

prop_PositiveAfter_Fails :: POSIXTime -> Integer -> Property
prop_PositiveAfter_Fails d r = (r > 0 && d < 999) ==> runChecks False d r

prop_NegativeAfter_Succeeds :: POSIXTime -> Integer -> Property
prop_NegativeAfter_Succeeds d r = (r < 0 && d < 999) ==> runChecks True d r 

runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldConsume deadline redeemer =
    --collect (redeemer, getPOSIXTime deadline) $ monadic property check
    monadic property check
        where check = do
                balancesMatch <- run $ testValues shouldConsume deadline redeemer
                assert balancesMatch

testValues :: Bool -> POSIXTime -> Integer -> Run Bool
testValues shouldConsume datum redeemer = do
    [ u1, u2 ] <- setUpUsers

    let val = adaValue 100
    sp <- spend u1 val
    submitTx u1 $ lockingTx datum sp val

    waitUntil waitBeforeConsumingTx

    utxos <- utxoAt valScript
    let [(oRef, oOut)] = utxos
    let tx = consumingTx datum redeemer u2 oRef (txOutValue oOut)
    let v2Expected = if shouldConsume then adaValue 1100 else adaValue 1000
    ct <- currentTimeRad 100
    tx' <- validateIn ct tx
    if shouldConsume then submitTx u2 tx' else mustFail . submitTx u2 $ tx'

    [ v1, v2 ] <- mapM valueAt [ u1, u2 ]
    return $ v1 == adaValue 900 && v2 == v2Expected