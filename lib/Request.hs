{-# LANGUAGE FlexibleInstances #-}

-- | How to execute requests to the GitHub API.
-- This module is meant to be used qualified.
module Request
  ( EvalResult (..),
    REST (..),
    RESTInput (..),
  )
where

import Control.Exception.Base (throwIO)
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.List (partition)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Exception (SpremutaException (..), mkBadBody)
import Log
import Network.HTTP.Simple (Request)
import qualified Network.HTTP.Simple as C
import qualified RequestMaker
import System.Process.Extra (spawnProcess)
import Types
import Prelude hiding (log)

class REST a b where
  eval :: (MonadIO m, MonadLogger m) => a -> m b

-- | How to handle a task after it has been passed to @eval@, when
-- in daemon mode.
data EvalResult
  = -- | In daemon mode: task should be reexecuted at next tick
    KeepMe
  | -- | In daemon mode: task has been completed, and should be removed
    RemoveMe

-- | Data passed to the main 'REST' instance.
data RESTInput = RestInput
  { -- | How to authentify to the VCS backend
    auth :: Maybe VCSAuth,
    -- | The options passed to the CLI
    options :: Options,
    -- | The task to execute
    task :: Task
  }

-- | Data passed to the 'REST' instance dealing with 'Condition'
data RESTConditionInput = RESTConditionInput
  { -- | How to authentify to the VCS backend
    auth :: Maybe VCSAuth,
    -- | The condition to check
    condition :: Condition
  }

instance REST RESTInput EvalResult where
  eval RestInput {auth, options, task = t@(Task todo condition)} =
    case todo of
      Merge _pr -> error "TODO"
      Notify -> do
        conditionTruth :: Bool <- eval $ RESTConditionInput {auth, condition}
        if conditionTruth
          then do
            let notifyMsg = case condition of
                  TrueCond -> "True 🤷"
                  HasCIFinished pr -> "CI of " ++ show pr ++ " is finished"
                  IsMerged pr -> show pr ++ " is merged"
                  HasGreenCI pr -> show pr ++ " has green CI"
            notify options notifyMsg
            return RemoveMe
          else do
            let logMsg = case condition of
                  TrueCond -> error "\"when true\" should always yield a notification"
                  HasCIFinished pr -> "CI of " ++ show pr ++ " is not finished"
                  IsMerged pr -> show pr ++ " is not merged"
                  HasGreenCI pr -> show pr ++ " has red CI"
            log logMsg
            log $ "Keeping task: " <> show t
            return KeepMe
      SetReady _pr -> error "TODO"

notify :: (MonadIO m, MonadLogger m) => Options -> String -> m ()
notify Options {notifyCmd} msg =
  case notifyCmd of
    Nothing -> do
      verbose "No notify command specified, writing on stdout"
      liftIO $ putStrLn msg
    -- TODO @smelc avoid this case, by having type @Maybe (NonEmpty String)@ for
    -- the notify command in the Options
    Just [] -> do
      log "notify command is empty, writing on stdout instead"
      liftIO $ putStrLn msg
    Just (program : args) -> do
      -- Calling spawnProcess: we don't want to wait for this program to finish
      void $ liftIO $ spawnProcess program (args ++ [msg])

instance REST RESTConditionInput Bool where
  eval rci@RESTConditionInput {condition} =
    case condition of
      TrueCond -> return True
      HasCIFinished pr -> go pr.vcs
      IsMerged pr -> go pr.vcs
      HasGreenCI pr -> go pr.vcs
    where
      go vcs =
        case vcs of
          GitHub -> evalGitHubCondition rci
          GitLab -> error "REST Condition Bool: GitLab not supported"

evalGitHubCondition :: (MonadIO m, MonadLogger m) => RESTConditionInput -> m Bool
evalGitHubCondition RESTConditionInput {auth, condition} =
  case condition of
    TrueCond -> return True
    HasCIFinished pr -> go HasCIFinishedKind pr
    IsMerged pr -> go IsMergedKind pr
    HasGreenCI pr -> go HasGreenCIKind pr
  where
    go :: (MonadIO m, MonadLogger m) => ConditionKind -> PR -> m Bool
    go cond pr@PR {owner, repo, vcs} = do
      body :: GitHubPR <- liftIO $ RequestMaker.make auth pr >>= evalRequest
      case cond of
        HasCIFinishedKind -> do
          let sha = body.head.sha
              cr = CheckRuns {owner, repo, sha, vcs}
          body' :: GitHubCheckRuns <- liftIO $ RequestMaker.make auth cr >>= evalRequest
          let (finished, pending) =
                bimap length length $
                  partition (\ghCheckRun -> isJust $ ghCheckRun.conclusion) body'.check_runs
              total = finished + pending
          log $ "CI progress for " <> show pr <> ": " <> show finished <> "/" <> show total
          return $ pending == 0
        IsMergedKind ->
          return body.merged
        HasGreenCIKind -> do
          let sha = body.head.sha
              cr = CheckRuns {owner, repo, sha, vcs}
          body' :: GitHubCheckRuns <- liftIO $ RequestMaker.make auth cr >>= evalRequest
          return $ all (\ghCheckRun -> ghCheckRun.conclusion == Just "success") body'.check_runs

-- | Throws a 'SpremutaException' if the status is not 200. Handle some codes
-- in a special way.
handleStatus :: (Show a, MonadIO m) => C.Request -> C.Response a -> m ()
handleStatus req response =
  case C.getResponseStatusCode response of
    200 -> return ()
    403 -> liftIO $ throwIO $ Request403 req
    404 -> liftIO $ throwIO $ Request404 req
    _ -> liftIO $ throwIO $ ResponseKO req response

-- | Performs a HTTP request and returns the body. Can throw a 'SpremutaException'.
evalRequest :: forall a m. (Aeson.FromJSON a, Show a, MonadIO m, MonadLogger m) => Request -> m a
evalRequest request = do
  response :: C.Response LBS.ByteString <- C.httpLBS request
  handleStatus request response
  let bodyBS :: LBS.ByteString = C.getResponseBody response
      errOrBody :: Either String a = Aeson.eitherDecode bodyBS
  body <-
    case errOrBody of
      Left err -> do
        -- Failed deserializing to type 'a'. Let's try to deserialize to Aeson.Value
        -- to log a more readable value. It's important for ease of debugging.
        debug $ case Aeson.eitherDecode bodyBS of
          Left _ ->
            -- Can't do better than showing raw body
            TL.unpack $ TL.decodeUtf8 bodyBS
          Right (value :: Aeson.Value) ->
            -- Can show formatted JSON
            showValue value
        badBody <- Exception.mkBadBody request err
        liftIO $ throwIO badBody
      Right b -> pure b
  let value :: Aeson.Value = seq body (Aeson.decode bodyBS) & fromJust
  debug $ showValue value
  return body
  where
    showValue x = TL.unpack . TL.decodeUtf8 $ Aeson.encodePretty x
