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

module Week06.Token.OnChain
    ( tokenPolicy
    , tokenCurSymbol
    , MintParams (..)
    , Amount (..)
    ) where

import           Ledger               hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import qualified Prelude              (Eq (..), Show (..))

newtype Amount = Amount { unAmount :: Integer }
                  deriving (Prelude.Show)
PlutusTx.makeLift ''Amount

instance Eq Amount where
    a1 == a2 = unAmount a1 == unAmount a2

data MintParams = MintParams
                { mpRef       :: !TxOutRef
                , mpTokenName :: !TokenName
                , mpAmount    :: !Amount
                }
                deriving (Prelude.Show, Eq)

PlutusTx.makeLift ''MintParams

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: MintParams -> () -> ScriptContext -> Bool
mkTokenPolicy (MintParams oref tn amt) () ctx =  traceIfFalse "UTxO not consumed"   hasUTxO
                                              && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && Amount amt' == amt
        _                -> False

tokenPolicy :: MintParams -> Scripts.MintingPolicy
tokenPolicy params@(MintParams oref tn amt) = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkTokenPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params

tokenCurSymbol :: MintParams -> CurrencySymbol
tokenCurSymbol = scriptCurrencySymbol . tokenPolicy
