module Main
    ( main
    ) where

import           Control.Exception    (throwIO)
import           Data.String          (IsString (..))
import           System.Environment   (getArgs)
import           Week06.Token.OnChain (Amount (..), MintParams (..),
                                       tokenPolicy)
import           Week06.Utils         (unsafeReadTxOutRef, writeMintingPolicy)

main :: IO ()
main = do
    [file, oref', amt', tn'] <- getArgs
    let oref = unsafeReadTxOutRef oref'
        amt  = read amt'
        tn   = fromString tn'
        p    = tokenPolicy . MintParams oref tn . Amount $ amt
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()
