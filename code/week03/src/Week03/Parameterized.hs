{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Parameterized where

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

-- Instead of a fixed datum, we can parameterize. The old name VestingDatum is
-- no longer appropriate. This type is now renamed VestingParam.
data VestingParam = VestingParam
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

-- Making the datum into IsData is no longer needed
-- PlutusTx.unstableMakeIsData ''VestingDatum

-- Necessary for the template Haskell when making the validator below on line 94
PlutusTx.makeLift ''VestingParam

{-# INLINABLE mkValidator #-}

-- We need to add another parameter. Before we had Datum, Redeemer, Context. Now
-- it's similar but Datum is replaced by VestingParam which itself needs
-- parameters.
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool

-- We replace the datum previously named `dat` with `p ()`
mkValidator p () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                          traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Replace dat with p
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary p

    -- Replace dat with p
    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline p) $ txInfoValidRange info

-- This type must change, too. The datum type is no longer VestingDatum, but is
-- instead unit ()
data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

-- typedValidator takes an additional parameter now, of VestingParam type
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting

-- Name the parameters p
typedValidator p = Scripts.mkTypedValidator @Vesting

    -- You might expect `$$(PlutusTx.compile [|| mkValidator p ||])` to work,
    -- but the inline pragma requires that everything inside the oxford brackets
    -- (||) must be known at compile time

    -- Instead, we must use the syntax below with applyCode and liftCode
    -- It's possible to lift p here because of the makeLift on line 50 above
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where

    -- To match the Vesting type, datum is no longer VestingDatum, instead ()
    wrap = Scripts.wrapValidator @() @()

-- validator now takes the VestingParam as a param
validator :: VestingParam -> Validator

-- Short (pointfree) version of
-- validator p = Scripts.validatorScript $ typedValidator p
validator = Scripts.validatorScript . typedValidator

-- valHash has the same changes as validator above
valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

-- scrAddress has the same changes as validator and valHash above
scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator

-- The off-chain code below changed, as well, to facilitate the parameterized
-- datum. It does mostly the same as before, but instead of using constant
-- beneficiaries and deadlines, those are grabbed via parameters

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" POSIXTime

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p  = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    if now < d
        then logInfo @String $ "too early"
        else do
            let p = VestingParam
                        { beneficiary = pkh
                        , deadline    = d
                        }
            utxos <- utxosAt $ scrAddress p
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos      <>
                                  Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                                  Constraints.mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
