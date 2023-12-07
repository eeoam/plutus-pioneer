module Pioneer.Week06.PSM where

import Control.Monad 
    ( replicateM
    )

import Test.Tasty (defaultMain, testGroup)

import Plutus.Model
    ( Ada (Lovelace)
    , Run
    , ada
    , adaValue
    , defaultBabbage
    , mustFail
    , newUser
    , noErrors
    , sendValue
    , testNoErrors
    , valueAt
    )

import Plutus.V1.Ledger.Api
    ( PubKeyHash
    )

setUpUsers :: Run [ PubKeyHash ]
setUpUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

simpleSpend :: Run Bool
simpleSpend = do
    users <- setUpUsers
    let [ u1, u2, u3 ] = users
    sendValue u1 (adaValue 100) u2
    sendValue u2 (adaValue 100) u3
    isOk <- noErrors
    vals <- mapM valueAt users
    return $ isOk && (vals == fmap adaValue [ 900, 1000, 1100 ])

notEnoughFunds :: Run Bool
notEnoughFunds = do
    users <- setUpUsers
    let [ u1, u2, _u3 ] = users
    sendValue u1 (adaValue 10000) u2
    noErrors

main :: IO ()
main = defaultMain $ do
    testGroup
        "Test simple user transactions"
        [ good "Simple spend" simpleSpend
        , bad "Not enough funds" notEnoughFunds
        ]
        where
            bad msg = good msg. mustFail
            good = testNoErrors (adaValue 10_000_000) defaultBabbage



