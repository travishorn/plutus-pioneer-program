{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

-- Week04.Vesting is a copy of week 3's Vesting.hs. Import it here.
import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

-- Test is an IO monad that runs runEmulatorTraceIO on myTrace. myTrace is
-- defined below
test :: IO ()
test = runEmulatorTraceIO myTrace

-- This EmulatorTrace is passed to runEmulatorTraceIO above. It is basically the
-- same example of emulating the vesting contract like we did manually in the
-- Playground last week
myTrace :: EmulatorTrace ()

-- Using do notation to bind together IO actions
myTrace = do
    -- Activate the contract on wallet 1. h1 will be a "contract handle".
    -- activateContractWallet is a monadic function that has the side effect of
    -- activating the contract
    h1 <- activateContractWallet (knownWallet 1) endpoints

    -- Do the same for wallet 2
    h2 <- activateContractWallet (knownWallet 2) endpoints

    -- Call the give endpoint with wallet 1 with parameters. These are the same
    -- parameters we're familiar with from week 3. Giving 10 ADA to wallet 2
    -- with a deadline of slot 20
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }

    -- Wait until slot 20. The return value is the slot number itself but we use
    -- void to ignore it since we don't need it here
    void $ waitUntilSlot 20

    -- To test a failing condition, you could wait only until slot 10 which is
    -- before the deadline
    -- void $ waitUntilSlot 10

    -- Call the grab endpoint with wallet 2
    callEndpoint @"grab" h2 ()

    -- Wait 2 slots. This time store the return value as `s`. `s` now contains
    -- the slot number
    s <- waitNSlots 2

    -- Log the trace
    Extras.logInfo $ "reached " ++ show s
