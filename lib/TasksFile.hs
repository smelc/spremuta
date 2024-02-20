-- | Module with modifications to the tasks text file, usually
-- named @spremuta.tasks@.
module TasksFile
  ( FileOps (..),
    isCommented,
    mapTask,
    mapTask',
    MappingEffect (..),
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import System.Directory.Extra (doesFileExist)
import System.IO.Extra (readFileUTF8', writeFileUTF8)
import Types
import Prelude hiding (log, readFile, writeFile)

data FileOps f m = FileOps
  { fileExists :: f -> m Bool,
    readFile :: f -> m String,
    writeFile :: f -> String -> m ()
  }

-- | The effect of mapping a task
data MappingEffect
  = -- | No change
    Nop
  | -- | Line @n@ got mapped over
    Mapped Int
  deriving (Eq, Show)

fileOpsIO :: (MonadIO m) => FileOps FilePath m
fileOpsIO =
  FileOps
    { fileExists = \f -> liftIO $ doesFileExist f,
      readFile = \f -> liftIO $ readFileUTF8' f,
      writeFile = \f s -> void $ liftIO $ writeFileUTF8 f s
    }

-- | The production implementation of mapping over a single task in a tasks file:
-- @mapTask (\s -> "# " <> s) f task@ comments @task@ in @f@
mapTask ::
  (MonadIO m) =>
  -- | What to do on the task's line
  (String -> String) ->
  -- The file in which to comment the task
  FilePath ->
  -- | The task to remove
  TaskString ->
  -- | Whether the modification was done
  m MappingEffect
mapTask f tasksFile t = mapTask' f fileOpsIO tasksFile t

-- | The main implementation of modifying a single task in the tasks file,
-- used in production and in tests. For example,
-- @mapTask' (\s -> "# " ++ s) ops f task@ comments @task@ in @f@
mapTask' ::
  (Monad m) =>
  -- | What to do on the task's line
  (String -> String) ->
  -- | How to operate on @file@. Use 'fileOpsIO' to obtain a production instance.
  FileOps file m ->
  -- The file in which to map over the task
  file ->
  -- | The task to remove
  TaskString ->
  -- | Whether a modification was done
  m MappingEffect
mapTask' f fileOps tasksFile (TaskString taskStr) = do
  -- This check doesn't strictly guarantee the file will exist when we read it.
  -- But most of the time it will, which makes the UX better (error message instead
  -- of exception)
  fileExists <- fileOps.fileExists tasksFile
  -- We need strict reading, because we may write to the file soon, see
  -- https://stackoverflow.com/a/5053188
  tasks <- if fileExists then lines <$> fileOps.readFile tasksFile else pure []
  case commentTask 1 tasks of -- Line numbers start at 1
    (_, Nothing) -> do
      return Nop
    (toWrite, Just changed) -> do
      fileOps.writeFile tasksFile $ unlines toWrite
      return $ Mapped changed
  where
    -- @commentTasks lineNumber tasks@ comments the first occurence of @task@ in @tasks
    -- (if it occurs) and returns whether the number of the line changed (if there is a change)
    commentTask :: Int -> [String] -> ([String], Maybe Int)
    commentTask lineNumber tasks =
      case tasks of
        [] -> ([], Nothing)
        fst : rest | fst == taskStr -> (f taskStr : rest, Just lineNumber)
        fst : rest ->
          let (rest', mInt) = commentTask (lineNumber + 1) rest
           in (fst : rest', mInt)

-- | Whether a task string is commented out, i.e. whether there is a '#'
-- after whitespaces, before any other thing.
isCommented :: TaskString -> Bool
isCommented (TaskString taskStr) =
  case listToMaybe afterSpace of Just '#' -> True; _ -> False
  where
    afterSpace = dropWhile isSpace taskStr
