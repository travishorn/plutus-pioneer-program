{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
-- In addition to Text, we need unpack when handling the error
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
-- Need functions from this module to work with wallets
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp

    -- Make sure the contract doesn't stop when an error occurs. Catch and
    -- handle the error. This lets the script continue running even if the first
    -- pay errors with insufficient funds
    handleError (\err -> Contract.logError $ "caught: " ++ unpack err) $
      void $ submitTx tx

    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each
-- time with Wallet 2 as recipient, but with amounts given by the two arguments.
-- There should be a delay of one slot after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()

-- Use do notation to bind a sequence of monadic actions
payTrace x y = do
    -- Activate the contract with wallet 1 and bind it's handle to h1
    h1 <- activateContractWallet (knownWallet 1) payContract

    -- Call the pay endpoint with wallet 1. Params are:
    -- ppRecipient = the pub key hash for wallet 2
    -- ppLovelace  = the first value passed in when running the trace
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
        , ppLovelace  = x
        }

    -- Wait 1 slot
    void $ Emulator.waitNSlots 1

    -- Call the pay endpoint again. This time ppLovelace is the 2nd value passed
    -- in when running the trace
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2
        , ppLovelace  = y
        }

    -- Wait 2 more slots to let the transactions finalize
    void $ Emulator.waitNSlots 2


-- Run emulator trace. Wallet 1 gives wallet 2 10 ADA and then 20 ADA
payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

-- Run another emulator trace. Wallet 1 tries to give wallet 2 1000 ADA. It
-- fails, the error is handled, execution continues. Wallet 1 gives wallet 2 20
-- ADA.
payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
