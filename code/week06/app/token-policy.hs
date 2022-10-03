module Main
    ( main
    ) where

import           Control.Exception    (throwIO)
import           Data.String          (IsString (..))
import           System.Environment   (getArgs)
import           Week06.Token.OnChain (MintParams (..), tokenPolicy)
import           Week06.Utils         (unsafeReadTxOutRef, writeMintingPolicy)

main :: IO ()
main = do
    [file, oref', tn'] <- getArgs
    let oref = unsafeReadTxOutRef oref'
        tn   = fromString tn'
        p    = tokenPolicy
             . MintParams oref
             $ tn
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()
