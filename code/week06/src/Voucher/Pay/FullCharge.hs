{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- This is a Voucher contract, which gives to a Company
-- way to provide Payable funds to be payed to given Amount,
-- by Company Employee authorizing the opration with NFT
-- and spend to specified Provider Wallet,

module Voucher.Pay.FullCharge
  (
  ) where

import           Control.Lens                 (makeClassyPrisms, prism', review,
                                               view)
import           Control.Monad                (void)
import           Control.Monad.Error.Lens     (catching, throwing, throwing_)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Default                 (def)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Sum (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Tx
import           Ledger                       (Address, PubKeyHash,
                                               ScriptContext, unspentOutputs)
import           Ledger.Address               (PaymentPubKeyHash (..))
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.TimeSlot              as TimeSlot
import           Ledger.Tx                    (ChainIndexTxOut (..),
                                               ciTxOutValue)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value                 (AssetClass (..), Value)
import qualified Ledger.Value                 as Value
import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract.StateMachine (TxConstraints)
import qualified Plutus.V1.Ledger.Ada         as Ada
import           Plutus.V1.Ledger.Time        (POSIXTime)
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), Eq (..),
                                               mempty, (.), (<>))
import           Prelude                      (Eq (..), Maybe (..), Show (..),
                                               String, mempty, show, undefined,
                                               (.), (<>))
import           Text.Printf                  (printf)
import qualified Wallet.Emulator              as Emulator

-- | We need a small datum script
data VoucherParams = VoucherParams
             { vpPayTo      :: !PaymentPubKeyHash
              -- ^ Publick key of Voucher suplier wallet
              , vpEmployee  :: !PaymentPubKeyHash
              -- ^ Publick KEY of Employee wallet which will hold
              --   Payment Auth Token
              , vpCompany   :: !PaymentPubKeyHash
              -- ^ Company wallet public key
              --   NOTE: May not be needed if not required in
              --   Contract Validation explicitly
              , vpToken     :: !AssetClass
              -- ^ Payment Authorization Token asset
              , vpStartFrom :: !POSIXTime
              -- ^The date from which the Voucher is active
              , vpDeadline  :: !POSIXTime
              -- ^ Voucher deadline from which the locked funds
              --   are able to be claimed back by the Comapny
              } deriving (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''VoucherParams

-- | This value is used in Pay end point
newtype Amount = Amount
               { getAmount :: Integer
               } deriving stock (Eq, Show, Generic)
                 deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)
PlutusTx.makeLift ''Amount

-- Maybe not needed PlutusTx.unstableMakeIsData ''TSRedeemer
-- | To set scenario for init to lock Amount given into Company Wallet and  NFT to be transfered to Employee Wallet
data VoucherRedeemer = Set
                     | Pay Amount
                     deriving ( PlutusTx.ToData
                              , PlutusTx.FromData
                              , PlutusTx.UnsafeFromData
                              )
-- PlutusTx.unstableMakeIsData ''VoucherRedeemer
PlutusTx.makeLift ''VoucherRedeemer

--- Some Voucher Contract Error handling

-- | Captures all posible errors in our contracts
data VaucherPayError = VaucherError Text
                     | VaucherContractError ContractError
                    deriving Show

makeClassyPrisms ''VaucherPayError

instance AsContractError VaucherPayError where
    _ContractError = _VaucherContractError

instance AsVaucherPayError Text where
    _VaucherPayError = prism' (Tx.pack . show) $ const Nothing

-- | Captures errors VaucherPayError
handleVaucherError :: (AsVaucherPayError e) => Text -> Contract () s e ()
handleVaucherError = logInfo . ("Error encoutered: " <> )

-- | Catch VaucherContractErrors
catchVaucherContractError :: AsVaucherPayError  e => Contract () s e () -> Contract () s e ()
catchVaucherContractError c = catching _VaucherContractError c (handleVaucherError . Tx.pack . show)


{-# INLINABLE validatePay #-}
-- | Validate Pay action
validatePay :: VoucherParams -> ScriptContext -> Bool
validatePay = undefined

{-# INLINABLE validateSpend #-}
-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: () -> VoucherRedeemer -> ScriptContext -> Bool
validateSpend () r _ctx = case r of
    Set -> True
    Pay amt  ->
      traceIfFalse "Wrong pin given: " $ r == 42

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

data Voucher
instance Scripts.ValidatorTypes Voucher where
    type instance RedeemerType Voucher = VoucherRedeemer
    type instance DatumType Voucher = ()

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Voucher
starterInstance = Scripts.mkTypedValidator @Voucher
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @VoucherRedeemer

--- OffChain Code ---

data SetParams = SetParams
               { spLockedAmount :: !Amount
               , spEmployeeKey  :: !PaymentPubKeyHash
               , spToken        :: !Value
               } deriving stock (Show, Eq, Generic)
                deriving anyclass (ToJSON, FromJSON, ToSchema)
-- | Params to be used by pay endpoint
data PayParams = PayParams
               { ppAmount :: !Amount
               , ppToken  :: !Value
               } deriving stock (Show, Eq, Generic)
                 deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)


-- | The schema of the contract, with two endpoints.
type Schema =   Endpoint "set" SetParams
            .\/ Endpoint "pay" Amount

contract :: AsContractError e => VoucherParams -> Contract () Schema e ()
contract vp = selectList [set, pay vp] >> contract vp

-- | The "set" contract endpoint locks funds in the contract and sends an
-- NFT to Employee wallet
set :: AsContractError e => Promise () Schema e ()
set = endpoint @"set" $ \ SetParams{spLockedAmount, spEmployeeKey, spToken} -> do
    let amountInt = getAmount spLockedAmount
        lockedValue = Ada.lovelaceValueOf amountInt
        tx =  Constraints.mustPayToTheScript () lockedValue
           <> Constraints.mustPayToPubKey spEmployeeKey spToken
    void $ submitTxConstraints starterInstance tx
    logInfo @String . printf "Voucher for %d lovlace available!" $ amountInt


-- | The "redeem" contract endpoint.
pay :: AsContractError e => VoucherParams -> Promise () Schema e ()
pay VoucherParams{vpToken, vpEmployee} = endpoint @"pay" $ \amount -> do
    unspentOutputs <- utxosAt contractAddress
    let currentlyLocked = foldMap (view ciTxOutValue) . Map.elems  $ unspentOutputs
        toBePayedValue  = Ada.lovelaceValueOf . getAmount $ amount
        remainingValue  = currentlyLocked - toBePayedValue
        -- NOTE: if there is still value NFT must be send to Employee
        remainingOutputs = mkScriptConstraints (vpEmployee, remainingValue, mkAuthToken vpToken)
        redeemer = Pay amount
        tx       = collectFromScript unspentOutputs redeemer
        txAmount = foldMap  _ciTxOutValue . Map.elems $ unspentOutputs
    -- void $ submitTxConstraintsSpending starterInstance unspentOutputs tx
    mkTxConstraints  (  Constraints.typedValidatorLookups starterInstance
                     <> Constraints.unspentOutputs unspentOutputs) tx
        >>= void . submitUnbalancedTx . Constraints.adjustUnbalancedTx
    logInfo @String $ printf "Voucher amount %s collected!" $ show txAmount


-- | make Auth Token from AssetClass ((CurrencySymbol, TokenName))
mkAuthToken :: AssetClass -> Value
mkAuthToken AssetClass{unAssetClass} = Value.singleton curSymbol tokenName 1
  where
    (curSymbol, tokenName) = unAssetClass

-- | Create posible script constrants locking remaing value into it and
-- sending NFT to owner back or just lock NFT into script if no value left
-- to spend
-- NOTE: Please consider min Value amount to be locked as few thousands
-- lovelace would not make sence to be left over
mkScriptConstraints :: (PaymentPubKeyHash, Value, Value) -> TxConstraints i ()
mkScriptConstraints (employeeHash, remainingValue, nftValue)
  | remainingValue `Value.gt`  mempty =  Constraints.mustPayToTheScript () remainingValue
                                    <> Constraints.mustPayToPubKey employeeHash  nftValue
  | otherwise  = Constraints.mustPayToTheScript () nftValue

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract defVoucherParams
  where
    slotZeroPosixTime = TimeSlot.scSlotZeroTime def
    paymKeyHash = Emulator.mockWalletPaymentPubKeyHash . Emulator.knownWallet
    defVoucherParams = VoucherParams
                     { vpPayTo = paymKeyHash 1
                     , vpEmployee = paymKeyHash 2
                     , vpCompany = paymKeyHash 3
                     , vpToken = AssetClass ("ff" ,"VTOC")
                     , vpStartFrom = slotZeroPosixTime
                     , vpDeadline = slotZeroPosixTime + 60000
                     }

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
