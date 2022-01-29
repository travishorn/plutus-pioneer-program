{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week03.Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeVestingValidator
    ) where

-- Use the Cardano API module. Contains all the functionality to interact with
-- the network
import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import           Week03.Parameterized

-- The Cardano API uses its own data type called ScriptData. It is very similar
-- to Plutus, but is its own type.

-- Helper function to convert Plutus data to ScriptData
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

-- Serializes Plutus data to JSON and writes it to a file
writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- Write unit () to a file called unit.json. This will be useful because our
-- datum and redeemer for Parameterized.hs are just unit ()
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

-- Write the validator to a file called vesting.plutus. We define the
-- beneficiary and the deadline here

-- Beneficiary will be wallet 2. We can get its public key hash with
-- cardano-cli address key-hash --payment-verification-key-file 02.vkey
-- The deadline can be found using an online POSIX time converter. Make sure
-- the deadline is in milliseconds. Set it to some time in the future. If you
-- want to test out a failed grab and then a successful one, maybe set it to one
-- hour in the future.
writeVestingValidator :: IO (Either (FileError ()) ())
writeVestingValidator = writeValidator "testnet/vesting.plutus" $ validator $ VestingParam
    { beneficiary = Ledger.PaymentPubKeyHash "d4f5fc7c64f0d0a90cf263b04c6af29c629e1d0674b6f83623800f1f"
    , deadline    = 1643413791000
    }
