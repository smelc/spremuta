{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | How to execute requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (REST (..)) where

import Control.Exception.Base (throwIO)
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Exception (SpremutaException (..))
import Log
import Network.HTTP.Simple
  ( Request,
  )
import qualified Network.HTTP.Simple as C
import qualified RequestMaker
import System.Process.Extra (spawnProcess)
import Types
import Prelude hiding (log)

class REST a b where
  eval :: (MonadIO m, MonadLogger m) => a -> m b

instance REST (Options, Task) () where
  eval (opts, Task todo cond) =
    case todo of
      Merge _pr -> error "TODO"
      Notify -> do
        val :: Bool <- eval cond
        let msg = case cond of
              TrueCond -> "True 🤷"
              IsMerged pr -> show pr ++ " is " ++ (if not val then "not " else "") ++ "merged"
              HasGreenCI pr -> show pr ++ " has " ++ (if val then "green" else "red") ++ " CI"
        notify opts msg
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

instance REST Condition Bool where
  eval cond =
    case cond of
      TrueCond -> return True
      IsMerged pr -> go pr.vcs cond
      HasGreenCI pr -> go pr.vcs cond
    where
      go vcs cond =
        case vcs of
          GitHub -> evalGitHubCondition cond
          GitLab -> error "REST Condition Bool: GitLab not supported"

evalGitHubCondition :: (MonadIO m, MonadLogger m) => Condition -> m Bool
evalGitHubCondition =
  \case
    TrueCond -> return True
    IsMerged pr -> go IsMergedKind pr
    HasGreenCI pr -> go HasGreenCIKind pr
  where
    go :: (MonadIO m, MonadLogger m) => ConditionKind -> PR -> m Bool
    go cond pr@PR {owner, repo, vcs} = do
      body :: GitHubPR <- liftIO $ RequestMaker.makePR pr >>= evalRequest
      case cond of
        IsMergedKind ->
          return body.merged
        HasGreenCIKind -> do
          let sha = body.head.sha
              cr = CheckRuns {owner, repo, sha, vcs}
          body' :: GitHubCheckRuns <- liftIO $ RequestMaker.makeCheckRuns cr >>= evalRequest
          return $ all (\ghCheckRun -> ghCheckRun.conclusion == "success") body'.check_runs

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
evalRequest :: forall a m. (Aeson.FromJSON a, Show a, MonadLogger m) => Request -> m a
evalRequest request = do
  response :: C.Response LBS.ByteString <- C.httpLBS request
  handleStatus request response
  let bodyBS :: LBS.ByteString = C.getResponseBody response
      errOrBody :: Either String a = Aeson.eitherDecode bodyBS
  body <-
    case errOrBody of
      Left err -> do
        debug $ TL.unpack $ TL.decodeUtf8 bodyBS
        liftIO $ throwIO (BadBody request err)
      Right b -> pure b
  let value :: Aeson.Value = seq body (Aeson.decode bodyBS) & fromJust
  verbose $ show body
  debug $ TL.unpack $ TL.decodeUtf8 $ Aeson.encodePretty value
  return body
