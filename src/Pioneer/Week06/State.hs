module Pioneer.Week06.State where

import Data.Function 
    ( (&)
    )
    
import Control.Monad.State
    ( State, get, put, runState
    )

data UTxO = UTxO { owner :: String, value :: Integer }
    deriving (Show, Eq)

newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]

sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    in if totalSenderFunds >= amount
        then (True, Mock $ [ receiverUtxo ] ++ [ senderChange ] ++ blockchainWithoutSenderUtxos)
        else (False, mockS)

multipleTx :: (Bool, Mock)
multipleTx =
    let (isOk, mockS1) = sendValue "Alice" 100 "Bob" initialMockS
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob" mockS1
        (isOk3, mockS3) = sendValue "Bob" 200 "Rick" mockS2
    in (isOk && isOk2 && isOk3, mockS3)

---

sendValue' :: String -> Integer -> String -> State Mock Bool
sendValue' from amount to = do
    mockS <- get
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
    let blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
    let totalSenderFunds = sum (map value senderUtxos)
    let receiverUtxo = UTxO to amount
    let senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [ receiverUtxo ] ++ [ senderChange ] ++ blockchainWithoutSenderUtxos
            return True
        else return False

multipleTx' :: (Bool, Mock)
multipleTx' = initialMockS & (runState $ do
                                isOk <- sendValue' "Alice" 100 "Bob"
                                isOk2 <- sendValue' "Alice" 300 "Bob"
                                isOk3 <- sendValue' "Bob"  200 "Rick"
                                return (isOk && isOk2 && isOk3))

type Run a = State Mock a

