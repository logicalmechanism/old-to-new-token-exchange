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
  , cdtConvRate  :: !Integer
  -- ^ The Converstion Rate Between The Two, usually 1
  , cdtNewPolicy :: !CurrencySymbol
  -- ^ The New Policy ID
  , cdtNewName   :: !TokenName
  -- ^ The New Token Name, in hex.
  , cdtIssuerPKH :: !PubKeyHash
  -- ^ The Token Issuer pkh
  }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType

-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
newtype CustomRedeemerType = CustomRedeemerType
  { crtDummy :: Integer } -- no redeemer required, use dummy.
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType

-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum _ context = checkTokenExchange
  where
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
        Just (Datum d)  -> case PlutusTx.fromBuiltinData d of
          Nothing       -> datum
          Just embedded -> embedded
    
    
    -- Check for a succuessful exchange
    checkTokenExchange :: Bool
    checkTokenExchange = txSignedBy info issuerPKH

    -- pubkeys
    issuerPKH :: PubKeyHash
    issuerPKH = cdtIssuerPKH datum

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