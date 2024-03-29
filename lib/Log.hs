module Log where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment (getArgs)
import Prelude hiding (log)

class (Monad m) => MonadLogger m where
  -- | Unconditionally log a line to stdout
  -- TODO @smelc: Change String to Text
  log :: String -> m ()

  -- | Log a line to stdout if '--verbose' has been has been specified on the command line
  -- TODO @smelc: Change String to Text
  verbose :: String -> m ()
  verbose s = do
    doIt <- hasVerbose
    when doIt (log s)

  -- | Log a line to stdout if '--verbose' or '--debug' has been specified on the command line
  -- TODO @smelc: Change String to Text
  debug :: String -> m ()
  debug s = do
    doIt <- hasDebug
    when doIt (log s)

  -- | Returns whether @verbose@ has an effect
  hasVerbose :: m Bool

  -- | Returns whether @debug@ has an effect
  hasDebug :: m Bool

instance MonadLogger IO where
  log = liftIO . putStrLn

  hasVerbose = do
    args <- liftIO getArgs
    return $ ("--debug" `elem` args || "--verbose" `elem` args)

  hasDebug = do
    args <- liftIO getArgs
    return $ "--debug" `elem` args

data LogLevel
  = Info
  | Verbose
  | Debug
  deriving (Show)
