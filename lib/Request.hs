{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | How to build requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (getPR, GetPRResponse(..)) where

import           Conduit             (MonadThrow)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Function       ((&))
import           GHC.Generics
import           Network.HTTP.Simple (Request, addRequestHeader, parseRequest)
import           Types               (PR (..), VCS (..))

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
