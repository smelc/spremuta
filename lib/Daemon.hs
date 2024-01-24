module Daemon (run) where

import Control.Monad.IO.Class
import Data.Ratio ((%))
import Log
import Numeric.Natural
import qualified Time.Units as T

-- | First parameter is the frequency with which to run (in minutes). Second parameter
-- is the path to the tasks file.
run :: (MonadIO m, MonadLogger m) => Natural -> FilePath -> m ()
run freq file = do
  runOnce freq file
  T.threadDelay $ T.minute (freq % 1)
  run freq file

runOnce :: (MonadIO m, MonadLogger m) => Natural -> FilePath -> m ()
runOnce = undefined
