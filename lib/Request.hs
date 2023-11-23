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
import           Types               (PRBits (..), VCS (..))

data GetPRResponse = GetPRResponse {
    url    :: String,
    number :: Int,
    state  :: String
  } deriving (Generic, Show)

instance ToJSON GetPRResponse where

instance FromJSON GetPRResponse where

-- | https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
getPR :: MonadThrow m => PRBits -> m Request
getPR (PRBits {owner, repo, number, vcs}) =
  case vcs of
    GitHub -> do
      req <- parseRequest $ "GET " <> url
      return $ setGHHeaders req
    GitLab -> error "getPR: GitLab not supported"
    where
      url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number

setGHHeaders :: Request -> Request
setGHHeaders req =
  req
    & addHeader app
    & addHeader version
  where
    addHeader (name, value) = addRequestHeader name value
    app = ("Accept", "application/vnd.github+json")
    version = ("X-GitHub-Api-Version", "2022-11-28")
