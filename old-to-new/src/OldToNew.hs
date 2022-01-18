{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

module OldToNew
  ( oldToNewScript
  , oldToNewScriptShortBs
  , Schema
  , contract
  , CustomDatumType(..)
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )
import           Control.Monad             ( void )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Prelude                   (String, (^))

import           Ledger                    hiding ( singleton )
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.CardanoWallet      as CW
import           Playground.Contract
import           Wallet.Emulator.Wallet    as W


import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.Builtins         as Bi
import qualified PlutusTx.Builtins.Internal as Internal
import           Plutus.Contract
import qualified Plutus.Trace.Emulator     as Trace
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0

  cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
  git rev 814df2c146f5d56f8c35a681fe75e85b905aed5d

-}

-------------------------------------------------------------------------------
-- | Create the contract parameters data object.
-------------------------------------------------------------------------------
data ContractParams = ContractParams {} -- no need for type yet
PlutusTx.makeLift ''ContractParams

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtOldPolicy :: !CurrencySymbol
  -- ^ The Old Policy ID
  , cdtOldName   :: !TokenName
  -- ^ The Old Token Name, in hex cli>1.33 
  , cdtNewPolicy :: !CurrencySymbol
  -- ^ The New Policy ID
  , cdtNewName   :: !TokenName
  -- ^ The New Token Name, in hex.
  , cdtIssuerPKH :: !PubKeyHash
  -- ^ The Token Issuer pkh
  , cdtMultiply  :: !Integer
  -- ^ The token multiplier for the exchange.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
  { crtAction    :: !Integer
  -- ^ The action as an integer in the redeemer.
  , crtExchanger :: !PubKeyHash
  -- ^ The user that is exchanging a token.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context
  | checkActionFlag = True
  | otherwise       = traceIfFalse "Validation Error" False
    where
      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      -- Either use an integer or use different constructors. What is best?
      checkActionFlag :: Bool
      checkActionFlag
        | actionFlag == 0 = exchange
        | actionFlag == 1 = remove
        | otherwise       = traceIfFalse "Incorrect Action Flag" True
          where
            actionFlag :: Integer
            actionFlag = crtAction redeemer

      -- | Put all the exchange functions together here
      exchange :: Bool
      exchange = do
        { let a = traceIfFalse "Embedded Datum Is Incorrect"         checkDatumForStateChange
        ; let b = traceIfFalse "Incorrect Value Returning To Script" $ checkContTxOutForValue scriptTxOutputs contValue
        ;         traceIfFalse "The Exchange Endpoint Has Failed"    $ all (==True) [a,b]
        }

      -- | Put all the remove functions together here
      remove :: Bool
      remove = do
        { let a = traceIfFalse "Incorrect Tx Signer"            $ txSignedBy info issuerPKH
        ; let b = traceIfFalse "Value Not Returning To Issuer"  $ Value.geq (valuePaidTo info issuerPKH) validatedValue
        ; let c = traceIfFalse "Spending Multiple Script UTxOs" oneScriptInput
        ;         traceIfFalse "The Remove Endpoint Has Failed" $ all (==True) [a,b,c]
        }

      -------------------------------------------------------------------------
      -- | TxInfo and TxOut Here
      -------------------------------------------------------------------------
      -- Get the Tx Info
      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = getContinuingOutputs context

      -- Find the new datum or return the old datum
      embeddedDatum :: [TxOut] -> CustomDatumType
      embeddedDatum [] = datum
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh info of
          Nothing         -> datum
          Just (Datum d)  -> Data.Maybe.fromMaybe datum (PlutusTx.fromBuiltinData d)

      -------------------------------------------------------------------------
      -- | Different Types of Data Here
      -------------------------------------------------------------------------
      -- values
      validatedValue :: Value
      validatedValue = case findOwnInput context of
        Nothing    -> traceError "No Input to Validate."  -- This should never be hit.
        Just input -> txOutValue $ txInInfoResolved input

      outboundValue :: Value
      outboundValue = valuePaidTo info exchangerPKH

      -- Internal.divideInteger amt 40

      contValue :: Value
      contValue = minimumRequiredAda                           <>
                  oldTokenValue (outboundOldAmt + oldValueAmt) <>
                  newTokenValue (newValueAmt - outboundNewAmt)
                    where
                      outboundOldAmt :: Integer
                      outboundOldAmt = Internal.divideInteger outboundNewAmt (cdtMultiply datum)

                      outboundNewAmt :: Integer
                      outboundNewAmt = Value.valueOf outboundValue (cdtNewPolicy datum) (cdtNewName datum)

                      minimumRequiredAda :: Value
                      minimumRequiredAda = Ada.lovelaceValueOf $ Value.valueOf validatedValue Ada.adaSymbol Ada.adaToken

                      oldValueAmt :: Integer
                      oldValueAmt = Value.valueOf validatedValue (cdtOldPolicy datum) (cdtOldName datum)

                      newValueAmt :: Integer
                      newValueAmt = Value.valueOf validatedValue (cdtNewPolicy datum) (cdtNewName datum)

                      oldTokenValue :: Integer -> Value
                      oldTokenValue amt = Value.singleton (cdtOldPolicy datum) (cdtOldName datum) amt

                      newTokenValue :: Integer -> Value
                      newTokenValue amt = Value.singleton (cdtNewPolicy datum) (cdtNewName datum) amt

      -- pubkeys
      issuerPKH :: PubKeyHash
      issuerPKH = cdtIssuerPKH datum

      exchangerPKH :: PubKeyHash
      exchangerPKH = crtExchanger redeemer

      -------------------------------------------------------------------------
      -- | Check Some Condition Functions Here
      -------------------------------------------------------------------------
      -- | Search each TxOut for the correct address and value.
      checkContTxOutForValue :: [TxOut] -> Value -> Bool
      checkContTxOutForValue [] _val = False
      checkContTxOutForValue (x:xs) val
        | Value.geq (txOutValue x) val = True
        | otherwise                    = checkContTxOutForValue xs val


      -- There is probably a better way to do this.
      checkDatumForStateChange :: Bool
      checkDatumForStateChange = do
        { let cdt = embeddedDatum scriptTxOutputs
        ; let a = traceIfFalse "Issuer PKH Can Not Change"      $ cdtIssuerPKH cdt == cdtIssuerPKH datum
        ; let b = traceIfFalse "Old Policy ID Can Not Change"   $ cdtOldPolicy cdt == cdtOldPolicy datum
        ; let c = traceIfFalse "Old Token Name Can Not Change"  $ cdtOldName   cdt == cdtOldName   datum
        ; let d = traceIfFalse "New Policy ID Can Not Change"   $ cdtNewPolicy cdt == cdtNewPolicy datum
        ; let e = traceIfFalse "New Token Name Can Not Change"  $ cdtNewName   cdt == cdtNewName   datum
        ; all (==True) [a,b,c,d,e]
        }

      -- Force a single utxo has a datum hash in the inputs by checking the length of the inputs that have a datum hash.
      oneScriptInput :: Bool
      oneScriptInput = traceIfFalse "More Than One Script Input Is Being Validated." $ loopInputs (txInfoInputs info) 0
        where
          loopInputs :: [TxInInfo] -> Integer -> Bool
          loopInputs []     !counter = counter == 1
          loopInputs (x:xs) !counter = case txOutDatumHash $ txInInfoResolved x of
              Nothing -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs counter
              Just _  -> do
                if counter > 1
                  then loopInputs [] counter
                  else loopInputs xs (counter + 1)


-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType

-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: ContractParams -> Scripts.TypedValidator Typed
typedValidator cp = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cp)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer

-------------------------------------------------------------------------------
-- | Define The Validator Here
-------------------------------------------------------------------------------
validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator $ ContractParams {})

-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Plutus.Script
script = Plutus.unValidatorScript validator

oldToNewScriptShortBs :: SBS.ShortByteString
oldToNewScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

oldToNewScript :: PlutusScript PlutusScriptV1
oldToNewScript = PlutusScriptSerialised oldToNewScriptShortBs

-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------

type Schema =
  Endpoint "endpoint"  CustomDatumType

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract