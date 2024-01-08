-- | How to build requests to the REST API.
module RequestMaker (makePR, makeCheckRuns) where

import Conduit (MonadThrow)
import Control.Monad.Except
import Data.Function ((&))
import Network.HTTP.Simple
  ( Request,
    addRequestHeader,
    parseRequest,
  )
import Types
import Prelude hiding (log)

-- | Returns a request to call
-- https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#get-a-pull-request
makePR :: (MonadThrow m) => PR -> m Request
makePR (PR {owner, repo, number, vcs}) =
  case vcs of
    GitHub -> do
      req <- parseRequest $ "GET " <> url
      return $ setHeaders' req
    GitLab -> error "getPR: GitLab not supported"
  where
    url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/pulls/" ++ show number
    setHeaders' = setHeaders vcs

-- | Returns a request to call
-- https://docs.github.com/en/rest/checks/runs?apiVersion=2022-11-28#list-check-runs-for-a-git-reference
makeCheckRuns :: (MonadThrow m) => CheckRuns -> m Request
makeCheckRuns (CheckRuns {owner, repo, sha, vcs}) =
  case vcs of
    GitHub -> do
      req <- parseRequest $ "GET " <> url
      return $ setHeaders' req
    GitLab -> error "getCheckRuns: GitLab not supported"
  where
    url = "https://api.github.com/repos/" ++ owner <> "/" ++ repo ++ "/commits/" ++ sha ++ "/check-runs"
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
