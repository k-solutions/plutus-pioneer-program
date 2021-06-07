{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.RockPaperScissors
    ( Game (..)
    , GameChoice (..)
    , GameParams (..)
    , FullParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx
import           Ledger.Value
import           Playground.Contract          (ToSchema)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup (..), check,
                                               unless)
import           Prelude                      (Semigroup (..))
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !Slot
    , gRevealDeadline :: !Slot
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = Rock
                | Paper
                | Scissors
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock  == Rock        = True
    Paper == Paper       = True
    Scissors == Scissors = True
    _ == _               = False
PlutusTx.unstableMakeIsData ''GameChoice

beats :: (GameChoice, GameChoice) -> Bool
beats (Rock, Scissors)  = True
beats (Scissors, Paper) = True
beats (Paper, Rock)     = True
beats _                 = False

type Nonce = ByteString
data GameDatum = GameDatum { gdPlayer     :: Nonce
                           , gdPlayerMove :: Maybe GameChoice
                           }
               | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs pm == GameDatum bs' pm' = (bs == bs') && (pm == pm')
    Finished           == Finished       = True
    _                  ==  _             = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice
                  | Reveal { rvPlayer :: Nonce, rvMove :: GameChoice }
                  | ClaimDraw
                  | ClaimFirst
                  | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

---  Start transition code ---

{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition Game{..} s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake           -> Just (  Constraints.mustBeSignedBy gSecond
                                                 <> Constraints.mustValidateIn (to gPlayDeadline)
                                                 ,  State (GameDatum bs $ Just c) (lovelaceValueOf dblGStake)
                                                 )
    (v, GameDatum _ (Just _), Reveal _ _)
        | lovelaces v == dblGStake       -> Just (  Constraints.mustBeSignedBy gFirst
                                                 <> Constraints.mustValidateIn (to gRevealDeadline)
                                                 <> cstPayToFirst
                                                 ,  finalState
                                                 )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake          -> Just (  Constraints.mustBeSignedBy gFirst
                                                 <> cstAfterDeadline
                                                 <> cstPayToFirst
                                                 ,  finalState
                                                 )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == dblGStake       -> Just (  Constraints.mustBeSignedBy gSecond
                                                 <> cstAfterDeadline
                                                 <> cstPayToFirst
                                                 ,  finalState
                                                 )
    _                                    -> Nothing
  where
    token :: Value
    token            = assetClassValue gToken 1

    cstAfterDeadline = Constraints.mustValidateIn (from $ 1 + gRevealDeadline)
    cstPayToFirst    = Constraints.mustPayToPubKey gFirst token
    finalState       = State Finished mempty
    dblGStake        = 2 * gStake

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine game@Game{..} = mkStateMachine (Just gToken) (transition game) final

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game = mkValidator $ gameStateMachine game

type Gaming = StateMachine GameDatum GameRedeemer

gameInst :: Game -> Scripts.ScriptInstance Gaming
gameInst game = Scripts.validator @Gaming
                ($$(PlutusTx.compile [|| mkGameValidator ||])
                `PlutusTx.applyCode` PlutusTx.liftCode game)
                $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . gameInst

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine game) (gameInst game)

--- Endpoints code ---

data GameParams = GameParams
    { gpKeyHash        :: !PubKeyHash
    , gpStake          :: !Integer
    , gpPlayDeadline   :: !Slot
    , gpRevealDeadline :: !Slot
    , gpCurrency       :: !CurrencySymbol
    , gpTokenName      :: !TokenName
    , gpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data FullParams = FullParams
                { fpParams :: !GameParams
                , fpNonce  :: !ByteString
                } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type GameSchema = BlockchainActions .\/ Endpoint "first" FullParams .\/ Endpoint "second" GameParams

mkGame :: (PubKeyHash, PubKeyHash) -> GameParams -> Game
mkGame (pkh1, pkh2) GameParams {..} = Game
              { gFirst          = pkh1
              , gSecond         = pkh2
              , gStake          = gpStake
              , gPlayDeadline   = gpPlayDeadline
              , gRevealDeadline = gpRevealDeadline
              , gToken          = AssetClass (gpCurrency, gpTokenName)
              }

mkChoice :: GameChoice -> ByteString
mkChoice c = case c of
               Paper    -> "0"
               Rock     -> "1"
               Scissors -> "2"

mapError' :: Contract  w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

firstGame :: forall w s. HasBlockchainActions s => FullParams -> Contract w s Text ()
firstGame FullParams{fpNonce = fpNonce, fpParams = fp@GameParams{..}} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = mkGame (pkh, gpKeyHash) fp
        client = gameClient game
        v      = lovelaceValueOf gpStake
        bs     = sha2_256 $ fpNonce `concatenate` mkChoice gpChoice
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " <> show gpChoice
    void $ awaitSlot $ 1 + gpPlayDeadline

    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> throwError "game output not found"
        Just ((o, _), _) -> case tyTxOutData o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c')
              | c' == gpChoice  -> do
                logInfo @String "draw"
                void $ mapError' $ runStep client $ Reveal fpNonce gpChoice
                logInfo @String "first player revealed and draw"

              | beats (c', gpChoice) -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal fpNonce gpChoice
                logInfo @String "first player revealed and won"

            _ -> logInfo @String "second player played and won"

secondGame :: forall w s. HasBlockchainActions s => GameParams -> Contract w s Text ()
secondGame sp@GameParams {..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = mkGame (gpKeyHash, pkh) sp
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just ((o, _), _) -> case tyTxOutData o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play gpChoice
                logInfo @String $ "made second move: " ++ show gpChoice

                void $ awaitSlot $ 1 + gpRevealDeadline

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won"
                    Just (state, _) -> do
                        logInfo @String $ "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            --  :wGameDatum _ (Just c')
            _ -> throwError "unexpected datum"

endpoints :: Contract () GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
