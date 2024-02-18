{-# LANGUAGE BangPatterns #-}

module Daemon
  ( Data (..),
    run,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Log
import Numeric.Natural
import qualified Parse
import qualified Request
import System.Directory.Extra (doesFileExist)
import System.IO.Extra (readFileUTF8')
import qualified TasksFile
import qualified Time.Units as T
import Types
import Prelude hiding (log)

-- | Data that is needed to execute the daemon. Name meant to be used qualified.
data Data = Data
  { -- | How to authenticate to the backend VCS
    auth :: Maybe VCSAuth,
    -- | Frequency at which to wake up (in minutes)
    frequency :: Natural,
    -- | The path to the tasks file
    tasksFile :: FilePath,
    -- | The  number of times the daemon woke up already. Initially at 0.
    tick :: Natural,
    -- | The options
    options :: Options
  }

run :: (MonadIO m, MonadLogger m) => Data -> m ()
run daemonData@Data {frequency = freq} = do
  runOnce daemonData
  T.threadDelay $ T.minute (freq % 1)
  run daemonData {tick = daemonData.tick + 1}

runOnce :: (MonadIO m, MonadLogger m) => Data -> m ()
runOnce daemonData@Data {tasksFile} = do
  -- This check doesn't strictly guarantee the file will exist when we read it.
  -- But most of the time it will, which makes the UX better (error message instead
  -- of exception)
  fileExists <- liftIO $ doesFileExist tasksFile
  -- We need strict reading, because we may write to the file soon, see
  -- https://stackoverflow.com/a/5053188
  !tasks <- if fileExists then liftIO $ lines <$> readFileUTF8' tasksFile else pure []
  case (fileExists, tasks) of
    (False, _) -> do
      log ("Tasks file does not exist: " <> tasksFile <> ". Nothing to do.")
      return ()
    (True, []) -> do
      log ("Tasks file is empty. Nothing to do.")
      return ()
    _ -> do
      let nbTasks = length tasks
          tasks' = filter (not . TasksFile.isCommented) $ map TaskString tasks
      case listToMaybe tasks' of
        Nothing -> do
          log ("All " <> show nbTasks <> " lines are commented out. Nothing to do.")
          return ()
        Just taskStr -> runOnceOnTask daemonData taskStr

-- | The continuation of @runOnce@ that deals with executing with the first
-- task (the second parameter) of the file. Spinoff of @runOnce@ to
-- keep the former small.
runOnceOnTask ::
  (MonadIO m, MonadLogger m) =>
  Data ->
  -- | The task to execute
  TaskString ->
  m ()
runOnceOnTask Data {auth, tasksFile, options} t@(TaskString taskStr) = do
  case Parse.parseAny Parse.pTask taskStr of
    Left parseError -> do
      log $ "Cannot parse task \"" <> taskStr <> "\": " <> parseError
      log $ "Commenting this task in tasks file: " <> tasksFile
      void $ TasksFile.commentTask tasksFile t
    Right task -> do
      let input = Request.RestInput {auth, options, task}
      _r :: Request.EvalResult <- Request.eval input
      -- TODO do something with the result
      return ()
