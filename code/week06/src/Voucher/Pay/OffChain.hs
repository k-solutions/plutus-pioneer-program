{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Voucher.Pay.OffChain where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Functor          (void)
import           Data.Text             (Text, unpack)
import           GHC.Generics          (Generic)
import           Ledger
import           Ledger.Ada            as Ada
import           Ledger.Constraints    as Constraints
import           Plutus.Contract       as Contract
import           Plutus.Contract.Test  (Wallet (Wallet), knownWallet,
                                        mockWalletPaymentPubKeyHash)
import           Plutus.Trace.Emulator as Emulator

--- OFFLINE Code ---

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) . lovelaceValueOf . ppLovelace $ pp
    Contract.handleError errHandler . void . submitTx $ tx
    payContract

errHandler :: Text -> Contract () PaySchema Text ()
errHandler = Contract.logInfo . ("Caught error: " <>)

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> EmulatorTrace ()
payTrace  amount = do
  h <- activateContractWallet (knownWallet 1) payContract
  let wallet2PubKeyHash = mockWalletPaymentPubKeyHash . knownWallet $ 2
      payParams :: Integer -> PayParams
      payParams = PayParams wallet2PubKeyHash
  callEndpoint @"pay" h . payParams $ amount
  void . Emulator.waitNSlots $ 1

  -- callEndpoint @ "pay" h . payParams $ amount2

--- TESTS ---

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000
