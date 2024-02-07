module Daemon
  ( Data (..),
    run,
  )
where

import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Log
import Numeric.Natural
import qualified Parse
import qualified Request
import System.Directory.Extra (doesFileExist)
import System.IO.Extra (readFileUTF8, writeFileUTF8)
import qualified Time.Units as T
import Types
import Prelude hiding (log)

-- | Data that is needed to execute the daemon. Name meant to be used qualified.
data Data = Data
  { -- | Frequency at which to wake up (in minutes)
    frequency :: Natural,
    -- | The path to the tasks file
    tasksFile :: FilePath,
    -- | The options
    options :: Options
  }

run :: (MonadIO m, MonadLogger m) => Data -> m ()
run daemonData@Data {frequency = freq} = do
  runOnce daemonData
  T.threadDelay $ T.minute (freq % 1)
  run daemonData

runOnce :: (MonadIO m, MonadLogger m) => Data -> m ()
runOnce daemonData@Data {tasksFile} = do
  -- This check doesn't strictly guarantee the file will exist when we read it.
  -- But most of the time it will, which makes the UX better (error message instead
  -- of exception)
  fileExists <- liftIO $ doesFileExist tasksFile
  tasks <- if fileExists then liftIO $ lines <$> readFileUTF8 tasksFile else pure []
  case (fileExists, tasks) of
    (False, _) -> do
      log ("Tasks file does not exist: " <> tasksFile <> ". Nothing to do.")
      return ()
    (True, []) -> do
      log ("Tasks file is empty. Nothing to do.")
      return ()
    _ -> do
      -- Not strictly necessary, but nice for the UX:
      tasks <- eatEmptyLines tasks tasksFile
      let mTaskStr = listToMaybe tasks
      case mTaskStr of
        Nothing -> return ()
        Just taskStr -> runOnceOnTask daemonData taskStr

-- | The continuation of @runOnce@ that deals with executing with the first
-- task (the second parameter) of the file. Spinoff of @runOnce@ to
-- keep the former small.
runOnceOnTask ::
  (MonadIO m, MonadLogger m) =>
  Data ->
  -- | The task to execute
  String ->
  m ()
runOnceOnTask _daemonData@Data {options} taskStr = do
  case Parse.parseAny Parse.pTask taskStr of
    Left _errorMsg ->
      undefined
    Right task -> do
      _r :: Request.EvalResult <- Request.eval (options, task)
      undefined

-- | @eatEmptyLines l f@ removes the empty lines from @f@ and returns the
-- lines after the empty lines.
eatEmptyLines ::
  (MonadIO m, MonadLogger m) =>
  -- | The lines of the file given as second argument
  [String] ->
  -- | The file from which the lines have been read
  FilePath ->
  -- | The lines after the empty lines
  m [String]
eatEmptyLines lines tasksFile = do
  if nbLinesRemoved == 0
    then return lines -- Nothing to do
    else do
      log ("Removing " <> show nbLinesRemoved <> " blank " <> lineStr nbLinesRemoved <> " fom tasks file: " <> tasksFile)
      liftIO $ writeFileUTF8 tasksFile $ unlines nonEmptyTail
      return nonEmptyTail
  where
    -- The lines after the empty lines
    nonEmptyTail :: [String] = dropWhile (all isSpace) lines
    nbLinesRemoved = length lines - length nonEmptyTail
    lineStr nb = if nb == 1 then "line" else "lines"
