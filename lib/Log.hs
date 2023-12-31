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
    args <- liftIO getArgs
    when ("--verbose" `elem` args) (log s)

  -- | Log a line to stdout if '--verbose' or '--debug' has been specified on the command line
  debug :: String -> m ()
  debug s = do
    args <- liftIO getArgs
    when ("--debug" `elem` args || "--verbose" `elem` args) (log s)

instance MonadLogger IO

data LogLevel
  = Info
  | Verbose
  | Debug
  deriving (Show)
