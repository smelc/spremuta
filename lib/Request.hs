{-# LANGUAGE MultiParamTypeClasses #-}

-- | How to build requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (getPR, REST (..)) where

import Conduit (MonadThrow)
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
    addRequestHeader,
    parseRequest,
  )
import qualified Network.HTTP.Simple as C
import Types
  ( Condition (..),
    ConditionKind (..),
    GitHubPR (..),
    PR (..),
    VCS (..),
  )
import Prelude hiding (log)

-- | https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
getPR :: (MonadThrow m) => PR -> m Request
getPR (PR {owner, repo, number, vcs}) =
  case vcs of
    GitHub -> do
      req <- parseRequest $ "GET " <> url
      return $ setHeaders' req
    GitLab -> error "getPR: GitLab not supported"
  where
    url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number
    setHeaders' = setHeaders vcs

setHeaders :: VCS -> Request -> Request
setHeaders vcs req =
  case vcs of
    GitHub ->
      req'
        & addHeader ("Accept", "application/vnd.github+json")
        & addHeader ("X-GitHub-Api-Version", "2022-11-28")
    GitLab -> error "SetHeaders: GitLab not supported"
  where
    addHeader (name, value) = addRequestHeader name value
    agent = ("User-Agent", "HTTP-Conduit")
    req' = addHeader agent req

class REST a b where
  eval :: (MonadIO m, MonadLogger m) => a -> m b

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

-- | Throws a 'SpremutaException' if the status is not 200. Handle some codes
-- in a special way.
handleStatus :: (Show a, MonadIO m) => C.Request -> C.Response a -> m ()
handleStatus req response =
  case C.getResponseStatusCode response of
    200 -> return ()
    403 -> liftIO $ throwIO $ Request403 req
    404 -> liftIO $ throwIO $ Request404 req
    _ -> liftIO $ throwIO $ ResponseKO req response

evalGitHubCondition :: (MonadIO m, MonadLogger m) => Condition -> m Bool
evalGitHubCondition =
  \case
    TrueCond -> return True
    IsMerged pr -> go IsMergedKind pr
    HasGreenCI pr -> go HasGreenCIKind pr
  where
    go :: (MonadIO m, MonadLogger m) => ConditionKind -> PR -> m Bool
    go cond pr = do
      request <- liftIO $ getPR pr
      response :: C.Response LBS.ByteString <- C.httpLBS request
      handleStatus request response
      let bodyBS :: LBS.ByteString = C.getResponseBody response
          errOrBody :: Either String GitHubPR = Aeson.eitherDecode bodyBS
      body <-
        case errOrBody of
          Left err -> do
            debug $ TL.unpack $ TL.decodeUtf8 bodyBS
            liftIO $ throwIO (BadBody request err)
          Right b -> pure b
      let value :: Aeson.Value = seq body (Aeson.decode bodyBS) & fromJust
      verbose $ show body
      debug $ TL.unpack $ TL.decodeUtf8 $ Aeson.encodePretty value
      case cond of
        IsMergedKind ->
          return body.merged
        HasGreenCIKind ->
          -- TODO
          return True
