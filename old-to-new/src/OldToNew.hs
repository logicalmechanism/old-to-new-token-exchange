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
  , createTheExchange
  , removeFromExchange
  , useTheExchange
  , Schema
  , contract
  , CustomDatumType(..)
  ) where
import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise           ( serialise )
import           Control.Monad ( void, forM_, when )
-- import           Control.Monad.Freer.Error ( throwError )

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import qualified Data.Maybe

import           Prelude                   (String)

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

import Plutus.Contracts.Currency qualified as Currency
import Data.Semigroup qualified as Semigroup
import Data.Void (Void)
import Plutus.Trace.Emulator qualified as Emulator
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

instance PlutusTx.Prelude.Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtOldPolicy a == cdtOldPolicy b) &&
           ( cdtOldName   a == cdtOldName   b) &&
           ( cdtNewPolicy a == cdtNewPolicy b) &&
           ( cdtNewName   a == cdtNewName   b) &&
           ( cdtIssuerPKH a == cdtIssuerPKH b) &&
           ( cdtMultiply  a == cdtMultiply  b)

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
        | otherwise       = traceIfFalse "Incorrect Action Flag" False -- This can be used as a bypass
          where
            actionFlag :: Integer
            actionFlag = crtAction redeemer

      -- | Put all the exchange functions together here
      exchange :: Bool
      exchange = do
        { let a = traceIfFalse "Embedded Datum Is Incorrect"         $ embeddedDatum $ getContinuingOutputs context
        ; let b = traceIfFalse "Incorrect Value Returning To Script" $ checkContTxOutForValue (getContinuingOutputs context) contValue
        ; let c = traceIfFalse "Incorrect Tx Signer"                 $ txSignedBy info exchangerPKH
        ; let d = traceIfFalse "Spending Multiple Script UTxOs"      $ loopInputs (txInfoInputs info) 0
        ;         traceIfFalse "The Exchange Endpoint Has Failed"    $ all (==True) [a,b,c,d]
        }

      -- | Put all the remove functions together here
      remove :: Bool
      remove = do
        { let a = traceIfFalse "Incorrect Tx Signer"                 $ txSignedBy info issuerPKH
        ; let b = traceIfFalse "Incorrect Value Returning To Issuer" $ checkTxOutForValueAtPKH (txInfoOutputs info) issuerPKH validatedValue
        ; let c = traceIfFalse "Spending Multiple Script UTxOs"      $ loopInputs (txInfoInputs info) 0
        ;         traceIfFalse "The Remove Endpoint Has Failed"      $ all (==True) [a,b,c]
        }

      -------------------------------------------------------------------------
      -- | TxInfo and TxOut Here
      -------------------------------------------------------------------------
      -- Get the Tx Info
      info :: TxInfo
      info = scriptContextTxInfo context

      -- Check for embedded datum in the txout
      embeddedDatum :: [TxOut] -> Bool
      embeddedDatum [] = False
      embeddedDatum (x:xs) = case txOutDatumHash x of
        Nothing -> embeddedDatum xs
        Just dh -> case findDatum dh info of
          Nothing         -> False
          Just (Datum d)  -> case PlutusTx.fromBuiltinData d of
            Nothing       -> False
            Just embedded -> embedded == datum
        

      -------------------------------------------------------------------------
      -- | Different Types of Data Here
      -------------------------------------------------------------------------
      -- values
      validatedValue :: Value
      validatedValue = case findOwnInput context of
        Nothing    -> traceError "No Input to Validate."  -- This should never be hit.
        Just input -> txOutValue $ txInInfoResolved input

      contValue :: Value
      contValue = minimumRequiredAda                           <>
                  oldTokenValue (outboundOldAmt + oldValueAmt) <>
                  newTokenValue (newValueAmt - outboundNewAmt)
                    where
                      outboundValue :: Value
                      outboundValue = valuePaidTo info exchangerPKH
                      
                      outboundNewAmt :: Integer
                      outboundNewAmt = Value.valueOf outboundValue (cdtNewPolicy datum) (cdtNewName datum)

                      outboundOldAmt :: Integer
                      outboundOldAmt = Internal.divideInteger outboundNewAmt (cdtMultiply datum)

                      oldValueAmt :: Integer
                      oldValueAmt = Value.valueOf validatedValue (cdtOldPolicy datum) (cdtOldName datum)

                      newValueAmt :: Integer
                      newValueAmt = Value.valueOf validatedValue (cdtNewPolicy datum) (cdtNewName datum)

                      oldTokenValue :: Integer -> Value
                      oldTokenValue amt = Value.singleton (cdtOldPolicy datum) (cdtOldName datum) amt

                      newTokenValue :: Integer -> Value
                      newTokenValue amt = Value.singleton (cdtNewPolicy datum) (cdtNewName datum) amt

                      minimumRequiredAda :: Value
                      minimumRequiredAda = Ada.lovelaceValueOf $ Value.valueOf validatedValue Ada.adaSymbol Ada.adaToken

      -- pubkeys
      issuerPKH :: PubKeyHash
      issuerPKH = cdtIssuerPKH datum

      exchangerPKH :: PubKeyHash
      exchangerPKH = crtExchanger redeemer

      -------------------------------------------------------------------------
      -- | Check Some Condition Functions Here
      -------------------------------------------------------------------------
      -- | Search each TxOut for the value.
      checkContTxOutForValue :: [TxOut] -> Value -> Bool
      checkContTxOutForValue [] _val = False
      checkContTxOutForValue (x:xs) val
        | checkVal  = True
        | otherwise = checkContTxOutForValue xs val
        where
          checkVal :: Bool
          checkVal = txOutValue x == val

      -- Search each TxOut for the correct address and value.
      checkTxOutForValueAtPKH :: [TxOut] -> PubKeyHash -> Value -> Bool
      checkTxOutForValueAtPKH [] _pkh _val = False
      checkTxOutForValueAtPKH (x:xs) pkh val
        | checkAddr && checkVal = True
        | otherwise             = checkTxOutForValueAtPKH xs pkh val
        where
          checkAddr :: Bool
          checkAddr = txOutAddress x == pubKeyHashAddress pkh

          checkVal :: Bool
          checkVal = txOutValue x == val
      
      -- Force a single utxo has a datum hash in the inputs by checking the length of the inputs that have a datum hash.
      loopInputs :: [TxInInfo] -> Integer -> Bool
      loopInputs []     !counter = counter == 1
      loopInputs (x:xs) !counter = case txOutDatumHash $ txInInfoResolved x of
          Nothing -> do
            if counter > 1
              then loopInputs [] counter -- quick end
              else loopInputs xs counter
          Just _  -> do
            if counter > 1
              then loopInputs [] counter -- quick end
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
-- The value being sold must be passed into the off chain data object.
data ExchangeDataType = ExchangeDataType
  { edtOldPolicy :: !CurrencySymbol
  -- ^ The Old Policy ID
  , edtOldName   :: !TokenName
  -- ^ The Old Token Name, in hex cli>1.33 
  , edtNewPolicy :: !CurrencySymbol
  -- ^ The New Policy ID
  , edtNewName   :: !TokenName
  -- ^ The New Token Name, in hex.
  , edtMultiply  :: !Integer
  -- ^ The token multiplier for the exchange.
  , edtNewTotal  :: !Integer
  -- ^ The token multiplier for the exchange.
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
type Schema =
  Endpoint "startExchange"  ExchangeDataType .\/
  Endpoint "removeExchange" CustomDatumType .\/
  Endpoint "useExchange"    CustomDatumType

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [startExchange,removeExchange, useExchange] >> contract

-- | Create some sample tokens
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- Plutus.Contract.ownPubKeyHash
    cur   <- Currency.mintContract ownPK [(tn, 1000) | tn <- tokenNames]
    logInfo @String "SetUp"

tokenNames :: [TokenName]
tokenNames = ["A"]


-- | The start endpoint.
startExchange :: AsContractError e => Promise () Schema e ()
startExchange =  endpoint @"startExchange" @ExchangeDataType $ \ExchangeDataType
  { edtOldPolicy = edtOldPolicy
  , edtOldName   = edtOldName
  , edtNewPolicy = edtNewPolicy
  , edtNewName   = edtNewName
  , edtMultiply  = edtMultiply
  , edtNewTotal  = edtNewTotal
  } -> do
    miner <- Plutus.Contract.ownPubKeyHash
    logInfo @String "Start a token exchange"
    logInfo @PubKeyHash miner
    let inst  = typedValidator $ ContractParams {}
        exchangeState = Ada.lovelaceValueOf 2000000 <> Value.singleton edtNewPolicy edtNewName edtNewTotal
        datum = CustomDatumType { cdtOldPolicy = edtOldPolicy
                                , cdtOldName   = edtOldName
                                , cdtNewPolicy = edtNewPolicy
                                , cdtNewName   = edtNewName
                                , cdtIssuerPKH = miner
                                , cdtMultiply  = edtMultiply
                                }
        constraint = Constraints.mustPayToTheScript datum exchangeState
    logInfo @String "Value"
    logInfo @Value exchangeState
    -- submit
    logInfo @String "submitting"
    ledgerTx <- submitTxConstraints inst constraint
    logInfo @String "awaiting"
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "The exchange has been started."

-- | The start endpoint.
removeExchange :: AsContractError e => Promise () Schema e ()
removeExchange =  endpoint @"removeExchange" @CustomDatumType $ \CustomDatumType {} -> do
  miner <- Plutus.Contract.ownPubKeyHash
  -- log some stuff
  logInfo @String "Remove a token exchange"
  logInfo @PubKeyHash miner

-- | The start endpoint.
useExchange :: AsContractError e => Promise () Schema e ()
useExchange =  endpoint @"useExchange" @CustomDatumType $ \CustomDatumType {} -> do
  miner <- Plutus.Contract.ownPubKeyHash
  -- log some stuff
  logInfo @String "Use a token exchange"
  logInfo @PubKeyHash miner



-------------------------------------------------------------------------------
-- | TRACES
-------------------------------------------------------------------------------

issuer :: CW.MockWallet
issuer = CW.knownWallet 1

exchanger :: CW.MockWallet
exchanger = CW.knownWallet 2

iPkh :: PubKeyHash
iPkh = CW.pubKeyHash issuer

ePkh :: PubKeyHash
ePkh = CW.pubKeyHash exchanger

-- IO calls for the repl
createTheExchange :: IO ()
createTheExchange = Trace.runEmulatorTraceIO createTheExchange'
  where
    createTheExchange' = do
      -- issuer holds the new tokens
      _ <- Emulator.activateContract (W.toMockWallet issuer) setupTokens "init"
      void $ Trace.waitNSlots 1
      
      -- exchanger holds the old tokens
      _ <- Emulator.activateContract (W.toMockWallet exchanger) setupTokens "init"
      void $ Trace.waitNSlots 1
      
      -- create the exchange
      hdl1 <- Trace.activateContractWallet (W.toMockWallet issuer) (contract @ContractError)
      void $ Trace.waitNSlots 1
      
      -- get values from minting
      
      -- create datum
      let datum = ExchangeDataType { edtOldPolicy = ""
                                   , edtOldName   = ""
                                   , edtNewPolicy = ""
                                   , edtNewName   = ""
                                   , edtMultiply  = 1
                                   , edtNewTotal  = 1000
                                   }
      Trace.callEndpoint @"startExchange" hdl1 datum
      void $ Trace.waitNSlots 1

-- | Testing removal
removeFromExchange :: IO ()
removeFromExchange = Trace.runEmulatorTraceIO createTheExchange'
  where
    createTheExchange' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet issuer) (contract @ContractError)
      let datum = CustomDatumType { cdtOldPolicy = ""
                                  , cdtOldName   = ""
                                  , cdtNewPolicy = ""
                                  , cdtNewName   = ""
                                  , cdtIssuerPKH = iPkh
                                  , cdtMultiply  = 1
                                  }
      Trace.callEndpoint @"removeExchange" hdl1 datum
      void $ Trace.waitNSlots 1

useTheExchange :: IO ()
useTheExchange = Trace.runEmulatorTraceIO createTheExchange'
  where
    createTheExchange' = do
      hdl1 <- Trace.activateContractWallet (W.toMockWallet issuer) (contract @ContractError)
      let datum = CustomDatumType { cdtOldPolicy = ""
                                  , cdtOldName   = ""
                                  , cdtNewPolicy = ""
                                  , cdtNewName   = ""
                                  , cdtIssuerPKH = iPkh
                                  , cdtMultiply  = 1
                                  }
      Trace.callEndpoint @"useExchange" hdl1 datum
      void $ Trace.waitNSlots 1