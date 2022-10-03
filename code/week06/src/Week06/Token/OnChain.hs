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
    ) where

import           Ledger               hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import qualified Prelude              (Show (..))

data MintParams = MintParams
                { mpRef       :: !TxOutRef
                , mpTokenName :: !TokenName
                }
                deriving (Prelude.Show)

instance Eq MintParams where
    (MintParams oref tn) == (MintParams oref' tn') = oref == oref' && tn == tn'

-- PlutusTx.unsafeFromBuiltinData ''MintParams
PlutusTx.makeLift ''MintParams

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: MintParams -> () -> ScriptContext -> Bool
mkTokenPolicy (MintParams oref tn) () ctx =  traceIfFalse "UTxO not consumed"   hasUTxO
                                          && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

tokenPolicy :: MintParams -> Scripts.MintingPolicy
tokenPolicy params = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkTokenPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params

tokenCurSymbol :: MintParams -> CurrencySymbol
tokenCurSymbol = scriptCurrencySymbol . tokenPolicy
