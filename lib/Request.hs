-- | How to build requests to the GitHub API.
module Request (getPR) where

import           Conduit             (MonadThrow)
import           Data.Function       ((&))
import           Network.HTTP.Simple (Request, addRequestHeader, parseRequest)
import           Types               (PRBits (..))

-- | https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
getPR :: MonadThrow m => PRBits -> m Request
getPR (PRBits {owner, repo, number}) = do
  _req <- parseRequest $ "GET " <> url
  undefined
  where
    url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number

_setGHHeaders :: Request -> Request
_setGHHeaders req =
  req
    & addHeader app
    & addHeader version
  where
    addHeader (name, value) = addRequestHeader name value
    app = ("Accept", "application/vnd.github+json")
    version = ("X-GitHub-Api-Version", "2022-11-28")
