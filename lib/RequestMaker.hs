-- | How to build requests to the REST API.
module RequestMaker (RequestMaker (..)) where

import Conduit (MonadThrow)
import Control.Monad.Except
import Data.ByteString.Char8 as BS
import Data.Function ((&))
import Network.HTTP.Simple
  ( Request,
    addRequestHeader,
    parseRequest,
    setRequestBasicAuth,
  )
import Types
import Prelude hiding (log)

-- | Class to build requests doing different things. If this prototype
-- doesn't suit you, you can also add a plain function for building a new
-- kind of request.
class RequestMaker a where
  make :: (MonadThrow m) => Maybe VCSAuth -> a -> m Request

instance RequestMaker PR where
  -- Returns a request to call
  -- https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
  make auth (PR {owner, repo, number, vcs}) =
    case vcs of
      GitHub -> do
        req <- parseRequest $ "GET " <> url
        return $ setHeaders' req
      GitLab -> error "getPR: GitLab not supported"
    where
      url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number
      setHeaders' = setHeaders vcs auth

instance RequestMaker CheckRuns where
  -- Returns a request to call
  -- https://docs.github.com/en/rest/checks/runs?apiVersion=2022-11-28#list-check-runs-for-a-git-reference
  make auth (CheckRuns {owner, repo, sha, vcs}) =
    case vcs of
      GitHub -> do
        req <- parseRequest $ "GET " <> url
        return $ setHeaders' req
      GitLab -> error "getCheckRuns: GitLab not supported"
    where
      url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/commits/" ++ sha ++ "/check-runs"
      setHeaders' = setHeaders vcs auth

setHeaders :: VCS -> Maybe VCSAuth -> Request -> Request
setHeaders vcs auth req =
  case vcs of
    GitHub ->
      req'
        & addHeader ("Accept", "application/vnd.github+json")
        & addHeader ("X-GitHub-Api-Version", "2022-11-28")
        & ( case auth of
              Nothing -> id
              Just (VCSAuth {user, pat}) ->
                setRequestBasicAuth (BS.pack user) (BS.pack pat)
          )
    GitLab -> error "SetHeaders: GitLab not supported"
  where
    addHeader (name, value) = addRequestHeader name value
    agent = ("User-Agent", "HTTP-Conduit")
    req' = addHeader agent req -- Base value for all VCS
