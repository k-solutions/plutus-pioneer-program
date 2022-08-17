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

module Week05.Homework2 where

import           Control.Applicative    (Const (Const))
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import           Plutus.V1.Ledger.Api   (Data (Constr))
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import           Prelude                (IO, Semigroup (..), Show (..), String,
                                         undefined)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

---  ONLINE Code  ---

data PolicyParams = PolicyParams
                  { ppOref      :: TxOutRef
                  , ppTokenName :: TokenName
                  } deriving Show
PlutusTx.makeLift ''PolicyParams

{-# INLINABLE mkPolicy #-}
-- | Minting policy for an NFT,
-- where the minting transaction must consume the given UTxO as input
-- and output it to given PubKey Address
-- where the TokenName will be the empty ByteString.
mkPolicy :: PolicyParams -> () -> ScriptContext -> Bool
mkPolicy (PolicyParams oref tn)  () (ScriptContext info _) =  traceIfFalse "UTxO not consumed"  hasUTxO &&                                           traceIfFalse "Has to be only One" isNFT
  where
    isNFT :: Bool
    isNFT = hasNFT . flattenValue . txInfoMint $ info

    hasNFT :: [(CurrencySymbol, TokenName, Integer)] -> Bool
    hasNFT ((_, tn', amt) : _) = tn == tn' && amt == 1
    hasNFT _                   = False


    hasUTxO :: Bool
    hasUTxO = any ((oref ==) . txInInfoOutRef) $ txInfoInputs info

policy :: PolicyParams -> Scripts.MintingPolicy
policy pp = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode` PlutusTx.liftCode pp

--- OFFLINE Code ---

curSymbol :: PolicyParams -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint adr = do
    utxos <- Contract.utxosAt adr
    case Map.keys utxos of
      []        -> Contract.logError @String  $ "no UTXOS found for address: " <> show adr
      oref : _  -> do
        let pp      = PolicyParams oref nftName
            nftVal  = Value.singleton (curSymbol pp) nftName 1
            lookups =  Constraints.mintingPolicy (policy pp)
                    <> Constraints.unspentOutputs utxos
            tx      =  Constraints.mustMintValue nftVal
                    <> Constraints.mustSpendPubKeyOutput oref
        ledgerTx <- submitTxConstraintsWith @Void lookups tx
        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
        Contract.logInfo @String $ printf "forged %s" $ show nftVal

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

--- Helpers ---

-- | We hard set NFT Token name
{-# INLINABLE nftName #-}
nftName = "VCH"


--- TEST ---

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
    callEndpoint @"mint" h2 $ mockWalletAddress w2
    void $ Emulator.waitNSlots 1
