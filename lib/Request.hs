-- | How to build requests to the GitHub API.
module Request (getPR) where

import           Conduit             (MonadThrow)
import           Network.HTTP.Simple (Request, parseRequest)
import           Types               (PRBits (..))

-- | https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
getPR :: MonadThrow m => PRBits -> m Request
getPR (PRBits {owner, repo, number}) = do
  _req <- parseRequest $ "GET " <> url
  undefined
  where
    url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number

-- setGHHeaders :: Request -> Request
-- setGHHeaders req =
--   undefined
--   where
--     _appHeader = "Accept: application/vnd.github+json"
--     _version = "X-GitHub-Api-Version: 2022-11-28"