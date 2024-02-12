-- | Module with modifications to the tasks text file, usually
-- named @spremuta.tasks@.
module TasksFile where

import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import Log
import System.Directory.Extra (doesFileExist)
import System.IO.Extra (readFileUTF8', writeFileUTF8)
import Types
import Prelude hiding (log)

commentTask ::
  (MonadIO m, MonadLogger m) =>
  -- | The task to remove
  TaskString ->
  -- The file in which to comment the task
  FilePath ->
  -- | Whether the modification was done
  m Bool
commentTask (TaskString taskStr) tasksFile = do
  -- This check doesn't strictly guarantee the file will exist when we read it.
  -- But most of the time it will, which makes the UX better (error message instead
  -- of exception)
  fileExists <- liftIO $ doesFileExist tasksFile
  -- We need strict reading, because we may write to the file soon, see
  -- https://stackoverflow.com/a/5053188
  tasks <- if fileExists then liftIO $ lines <$> readFileUTF8' tasksFile else pure []
  case commentTask tasks of
    (_, False) -> do
      log $ "Task not found in tasks file: " <> taskStr <> ". Cannot comment it. This is unexpected 🙁"
      return False -- Task not found
    (toWrite, True) -> do
      liftIO $ writeFileUTF8 tasksFile $ unlines toWrite
      return True
  where
    -- @commentTasks tasks@ comments the first occurence of @task@ in @tasks
    -- (if it occurs) and returns whether a change was done.
    commentTask :: [String] -> ([String], Bool)
    commentTask tasks =
      case tasks of
        [] -> ([], False)
        fst : rest | fst == taskStr -> ("# " <> taskStr : rest, True)
        fst : rest ->
          let (rest', b) = commentTask rest
           in (fst : rest', b)

-- | Whether a task string is commented out, i.e. whether there is a '#'
-- after whitespaces, before any other thing.
isCommented :: TaskString -> Bool
isCommented (TaskString taskStr) =
  case listToMaybe afterSpace of Just '#' -> True; _ -> False
  where
    afterSpace = dropWhile isSpace taskStr
