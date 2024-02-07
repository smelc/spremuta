module Log where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment (getArgs)
import Prelude hiding (log)

class (MonadIO m) => MonadLogger m where
  -- | Unconditionally log a line to stdout
  log :: String -> m ()
  log = liftIO . putStrLn

  -- | Log a line to stdout if '--verbose' has been has been specified on the command line
  verbose :: String -> m ()
  verbose s = do
    doIt <- hasVerbose
    when doIt (log s)

  -- | Log a line to stdout if '--verbose' or '--debug' has been specified on the command line
  debug :: String -> m ()
  debug s = do
    doIt <- hasDebug
    when doIt (log s)

  -- | Returns whether @debug@ has an effect
  hasVerbose :: m Bool
  hasVerbose = do
    args <- liftIO getArgs
    return $ "--verbose" `elem` args

  -- | Returns whether @verbose@ has an effect
  hasDebug :: m Bool
  hasDebug = do
    args <- liftIO getArgs
    return $ ("--debug" `elem` args || "--verbose" `elem` args)

instance MonadLogger IO

data LogLevel
  = Info
  | Verbose
  | Debug
  deriving (Show)
