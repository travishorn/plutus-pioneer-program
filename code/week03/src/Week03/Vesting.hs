-- This script is based on IsData.hs from lecture 2

-- Notice a few extra language extensions we need this time
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

-- Notice some additional imports, as well
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

-- Datum should contain the beneficiary and the deadline.
data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- Again, the validator takes datum, redeemer, and context. It returns a Bool.
-- Datum is the custom type we defined above. For the redeemer, the beneficiary
-- doesn't need to provide any additional information. So redeemer is simply
-- unit `()` in this case.
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool

-- Giving the datum, redeemer, and context names `dat`, `()`, and `ctx`
-- Only return True if signedByBeneficiary && deadlineReached. Both of those
-- functions are defined in the `where` section below. If either fail,
-- `traceIfFalse` will provide a trace with a friendly error message
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    -- `info` contains the TxInfo field of the script context. It makes sense to
    -- assign this to its own variable sense we use it more than once below
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Returns true if signed by the beneficiary. Otherwise false.
    -- `txSignedBy` takes the TxInfo of the context and a public key hash. We
    -- get the public key hash for the beneficiary by calling
    -- `unPaymentPubKeyHash` on the `beneficiary` field of the datum
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    -- The deadline interval should be any time FROM the deadline all the way TO
    -- infinity. We can describe that with `(from $ deadline dat)`

    -- The current time interval is the txInfoValidRange field of the context.
    -- This can be described as `txInfoValidRange info`

    -- Pass both of these as arguments to `contains` to make sure the current
    -- time interval is completely within the deadline interval.
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info


-- Some necessary boilerplate

-- Dummy type to encode the data types of datum and redeemer
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

-- Following the same scheme as IsData.hs from last week, we can compile our
-- validator into an actual Plutus validator.
typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

-- Get the validator, hash, and address for use in the off-chain code

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- Define the endpoints we want to expose to the user

-- The give endpoint will been a beneficiary public key, a deadline until which
-- the ADA cannot be collected before, and the amount of ADA to put in
data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Grab doesn't need any parameters. He just needs to grab from the matching
-- beneficiary public key after the deadline

-- Give is for the person who wants to set up the contract
-- Grab is for the beneficiary to pull ADA out of the contract
type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    -- Set up the datum with beneficiary and deadline. Simply read them from the
    -- give parameters (referenced here as gp)
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        -- For the tx itself, use a constraint to say we want a tx that has an
        -- output at this script address. Give the constraint the datum and the
        -- value. Get the value gpAmount from the give parameters gp and use
        -- Ada.lovelaceValueOf to convert to Lovelace (1 ADA = 1,000,000
        -- Lovelace)
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

-- Grab is more complicated that give because there may by many UTxOs that use
-- this same script address to vest for different people. The only ones suitable
-- for a person calling grab will be ones with their public key as the
-- beneficiary and where the deadline has already passed 

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    -- Grabber looks up the current time
    now   <- currentTime

    -- And their own public key hash
    pkh   <- ownPaymentPubKeyHash

    -- Get all the UTxOs at this address, but filter for only the ones that are
    -- suitable based on beneficiary and deadline. See isSuitable below
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress

    -- Check if any UTxOs exist in the filtered list, meaning they are suitable
    if Map.null utxos

        -- If the list is empty, no gifts available
        then logInfo @String $ "no gifts available"

        -- Otherwise, create a transaction that combines all of the suitable
        -- UTxOs into one tx. This could fail in the real world because if there
        -- are too many UTxOs, the transaction can get too big and be over tx
        -- size limit. We don't handle that case here
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          -- You cannot specify a tx is valid for an infinite
                          -- time. Instead we say it's valid starting now
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    -- Given a public key hash, a time, and a UTxO, return whether the UTxO is
    -- suitable for grabbing
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool

    -- Txs that have an output at the script address are not required to provide
    -- the datum. They can provide the datum hash instead. Check if datum is
    -- provided
    isSuitable pkh now o = case _ciTxOutDatum o of

        -- If no datum provided and instead all we have is the hash, simply
        -- return false since we can't know the beneficiary or the deadline of
        -- that UTxO. That UTxO is not suitable
        Left _          -> False

        -- If datum is provided, try to deserialize it to the appropriate datum
        -- data type
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of

            -- If it cannot be deserialized, it's not the right type, return
            -- False. The UTxO is not suitable
            Nothing -> False

            -- If all other checks fail, check whether the grabber is the
            -- beneficiary for this UTxO and that the deadline is less than or
            -- equal to now (is either in the past or the exact current time)
            Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
