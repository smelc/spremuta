{-# LANGUAGE DeriveGeneric        #-}

-- | How to build requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (getPR, GetPRResponse(..), VCSAPI(..)) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Function          ((&))
import           Data.Functor
import           GHC.Generics
import           Network.HTTP.Simple    (Request, addRequestHeader,
                                         parseRequest)
import qualified Network.HTTP.Simple    as C
import           Types                  (Condition (..), ConditionKind (..),
                                         PR (..), VCS (..))
import Conduit (MonadThrow)

data GetPRResponse = GetPRResponse {
    url    :: String,
    number :: Int,
    state  :: String
  } deriving (Generic, Show)

instance ToJSON GetPRResponse where

instance FromJSON GetPRResponse where

-- | https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
getPR :: MonadThrow m => PR -> m Request
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

class VCSAPI a where
  evalCondition :: MonadIO m => a -> Condition -> m Bool

instance VCSAPI VCS where
  evalCondition vcs cond =
    case vcs of
      GitHub -> evalGitHubCondition cond
      GitLab -> error "GitLab unsupported in evalCondition"

evalGitHubCondition :: MonadIO m => Condition -> m Bool
evalGitHubCondition =
  \case
    TrueCond -> return True
    IsMerged pr -> liftIO $ go IsMergedKind pr
    HasGreenCI pr -> liftIO $ go HasGreenCIKind pr
  where
    -- Can't make this MonadIO m => ... -> m Bool?
    -- without introducing a newtype
    -- (see https://stackoverflow.com/questions/41751854/why-am-i-getting-overlapping-instances-error-when-one-doesnt-match)
    go :: ConditionKind -> PR -> IO Bool
    go _ck pr = do
      req <- getPR pr
      resp :: Request.GetPRResponse <- C.httpJSON req <&> C.getResponseBody
      print resp
      return True

-- instance (Monad m, MonadIO m) => MonadThrow m where
--   throwM = liftIO . throwIO