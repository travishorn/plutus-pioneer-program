-- Copied language extensions from week 1 code
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

-- Copied imports from week 1 code
import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

-- The validator will ultimately be a script living on the block chain. But
-- first we write it in Haskell

-- Allow compiler to inline `mkValidator` inside brackets in template Haskell
{-# INLINABLE mkValidator #-}

-- Validator signature. Gets datum, reedemer, and context
-- Datum from output being consumed
-- Redeemer from input that is consuming
-- Context is transaction that is consuming
-- Return value is (), pronounced "unit".  It's similar to the void type in
-- other languages. Since there is no return value, as long as no error is
-- thrown, validation passes.
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()

-- Simplest validator possible. Ignores all 3 inputs and simply passes. If
-- someone puts ADA into a contract with this validator, *anyone* would be able
-- to get it out. They would use this script as an input and their wallet as an
-- output. The validator always passes so the tx will be successful.
mkValidator _ _ _ = ()

-- The actual validator. Uses template Haskell. You don't exactly have to know
-- what all of this means. This part uses the same pattern each time you'll want
-- to use it so it's more or less a copy-and-paste thing. `mkValidatorScript`
-- takes `CompiledCode` and produces a `Validator`. `CompiledCode` contains  an
-- expression that takes 3x `BuiltinData` and outputs a ().
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- The hash of the validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

-- Given the validator, turn it into a real address on the blockchain
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- The only really interesting line is the actual logic defined in `mkValidator`
-- (line 52 above). Everything else above is just boilerplate.

-- Below this line is the off-chain code that is beyond the scope of this
-- lecture. It is necessary to actually use the on-chain code we wrote above.

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

