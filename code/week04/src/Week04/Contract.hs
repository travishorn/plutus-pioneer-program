{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- EmulatorTrace a

-- The `Contract` monad takes 4 arguments: `w s e`

-- `w` is like the Writer we worked on earlier in this lecture. It allows you to
-- log messages of type `w`. The real purpose of logging messages is to
-- communicate between two contracts. The written `w` is visible to other
-- contracts.

-- `s` specifies the endpoints

-- `e` specifies the type of error message that you can throw and catch inside
-- the contract

-- `a` is the return value as always in Haskell


-- A new contract
-- We don't want to write any log messages so w is unit ()
-- We don't want any endpoints so s is Empty
-- For e, a popular choice is Text
-- We don't care about the result so it's unit () as well
myContract1 :: Contract () Empty Text ()

-- A simple contract that just logs a string. Note this isn't the same "logging"
-- that w normally does so other contracts can see the result. It just logs out
-- to the console when testing. If you run test1 in the REPL with this in place,
-- you'll see the log message at Slot 1
-- myContract1 = Contract.logInfo @String "hello from the contract"

-- Example contract of throwing an error. Remember that myContract1's type
-- signature specified that errors are of type Text. So something like "BOOM!"
-- works here. In this example, we're using do notation to sequence a logInfo
-- action after the error. But since we're actually throwing an error, that
-- action is never reached when test1 is run
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

-- An emulator trace to emulate myContract1
myTrace1 :: EmulatorTrace ()
-- All we have to do is use a wallet to activate the contract. The void part is
-- necessary because myTrace1's type signature is expecting a return valid of
-- (). If we don't use void here, the return type would be the return type of
-- activateContractWallet (which is handle)
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

-- A function to run myTrace1 using runEmulatorTraceIO
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- Similar to myContract1 except how we want to catch and handle the error.
-- Notice that e is now of type Void instead of (). Void has no values so you
-- cannot **throw** an exception with this method
myContract2 :: Contract () Empty Void ()

-- Contract.handleError takes two arguments: a handler and a contract
myContract2 = Contract.handleError
    -- The error handler that happens when an exception is thrown
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    -- Passing myContract1 to handleError. Remember that myContract1 above does
    -- indeed throw an error. That will get caught with handleError
    myContract1

-- The emulator trace for myContract2
myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

-- The function to run the emulator trace myTrace2. The contract execution will
-- succeed "without errors" now. That's because, even though there is an error,
-- it's actually caught, handled, and logged with logError
test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- Note that some errors still won't be caught with this method. For example, if
-- you constructed the transaction incorrectly, handleError won't catch that


-- Using the second argument s. This specifies the schema for the endpoints we
-- can invoke

-- Two endpoints: foo which is an Int and bar which is a String
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

-- Use mySchema as the 2nd argument
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    -- awaitPromise means that we're waiting for some outside stimulus to invoke
    -- the endpoint. We awaitPromise on the two possible endpoints.

    -- Wait for foo to be called. When it's called, pass the value to logInfo
    awaitPromise $ endpoint @"foo" Contract.logInfo
    -- Same for endpoint bar
    awaitPromise $ endpoint @"bar" Contract.logInfo

-- Trace for myContract3
myTrace3 :: EmulatorTrace ()
myTrace3 = do
    -- Activate the contract. Store wallet 1's handle as h
    h <- activateContractWallet (knownWallet 1) myContract3
    -- Call the foo endpoint with wallet 1's handle. Remember foo's type is int
    callEndpoint @"foo" h 42
    -- Similarly, call the bar endpoint
    callEndpoint @"bar" h "Haskell"

-- Function for running myTrace3. Running this, you'll see logInfo for endpoint
-- foo with value 42 and logInfo for endpoint bar with value "Haskell". Both
-- during slot 1.
test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- A final example. This time we're utilizing w which logs messages that can be
-- picked up by other contracts. w must be a monoid. The prime example of a
-- monoid is a List. We're using a list of Ints here.
-- s is Empty. No endpoints
-- e is Text. Errors can be of type Text
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    -- Wait 10 slots. This is the contract itself waiting. Not the emulator
    void $ Contract.waitNSlots 10
    -- Use tell to invoke the w writer. We're logging a list that contains a
    -- single Int 1
    tell [1]
    -- Wait another 10 slots
    void $ Contract.waitNSlots 10
    -- Invoke writer w again with list containing single Int 2 this time
    tell [2]
    -- Wait another 10 slots
    void $ Contract.waitNSlots 10

-- The trace for myContract4
myTrace4 :: EmulatorTrace ()
myTrace4 = do
    -- Activate the contract with wallet 1
    h <- activateContractWallet (knownWallet 1) myContract4

    -- Wait 5 slots in the emulator
    void $ Emulator.waitNSlots 5
    -- Use observableState to look up the state of a handle in a running
    -- contract. Store the results as xs
    xs <- observableState h
    -- logInfo on the xs to inspect them
    Extras.logInfo $ show xs

    -- Wait a further 10 slots and observe the state again
    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    -- Wait a further 10 slots and observe the state again
    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

-- A function to run myTrace4
test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4

-- The first time the trace observes the state, it does so in slot 5. But it
-- takes a couple slots to do so. So in slot 7, you'll see *** USER LOG: [].
-- That's the first logInfo with the state. At this point, the state is empty
-- because we're not `tell`ing until slot 10.

-- In slot 15 we observe the state again. We can view the output in slot 17. The
-- state is now *** USER LOG: [1] because the contract used `tell` in slot 10 to
-- log the Int 1.

-- Finally we observe state again at slot 25. We can view it in slot 27. The
-- state is now *** USER LOG: [1,2] since the contract used `tell` in slot 20 to
-- log the Int 2
