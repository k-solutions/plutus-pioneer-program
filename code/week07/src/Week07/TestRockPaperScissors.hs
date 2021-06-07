{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.TestRockPaperScissors where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.Value
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Wallet.Emulator.Wallet

import           Week07.RockPaperScissors

test :: IO ()
test = do
    test' Rock Rock
--    test' Rock Paper
--    test' One Zero
--    test' One One

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO' def emCfg $ myTrace c1 c2
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList
        [ (Wallet 1, v <> assetClassValue (AssetClass (gameTokenCurrency, gameTokenName)) 1)
        , (Wallet 2, v)
        ]

    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000

gameTokenCurrency :: CurrencySymbol
gameTokenCurrency = "ff"

gameTokenName :: TokenName
gameTokenName = "STATE TOKEN"

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    let pkh1 = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2 = pubKeyHash $ walletPubKey $ Wallet 2

        fp = GameParams
                { gpKeyHash        = pkh2
                , gpStake          = 5000000
                , gpPlayDeadline   = 5
                , gpRevealDeadline = 10
                , gpCurrency       = gameTokenCurrency
                , gpTokenName      = gameTokenName
                , gpChoice         = c1
                }
        sp = GameParams
                { gpKeyHash        = pkh1
                , gpStake          = 5000000
                , gpPlayDeadline   = 5
                , gpRevealDeadline = 10
                , gpCurrency       = gameTokenCurrency
                , gpTokenName      = gameTokenName
                , gpChoice         = c2
                }
        fullP = FullParams {fpParams = fp, fpNonce = "SECRETNONCE"}

    callEndpoint @"first" h1 fullP

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp

    void $ Emulator.waitNSlots 10
