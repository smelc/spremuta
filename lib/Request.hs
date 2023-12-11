{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | How to build requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (getPR, GetPRBody(..), REST(..)) where

import           Conduit                (MonadThrow)
import           Control.Exception      (throw)
import           Control.Monad.Except
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Function          ((&))
import           Exception              (SpremutaException (..))
import           GHC.Generics
import           Network.HTTP.Simple    (Request, addRequestHeader,
                                         parseRequest)
import qualified Network.HTTP.Simple    as C
import           Types                  (Condition (..), ConditionKind (..),
                                         PR (..), VCS (..))

data GetPRBody = GetPRBody {
    url    :: String,
    number :: Int,
    state  :: String
  } deriving (Generic, Show)

instance ToJSON GetPRBody where

instance FromJSON GetPRBody where

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

class REST a b where
  eval :: MonadIO m => a -> m b

instance REST Condition Bool where
   eval cond =
    case cond of
      TrueCond -> return True
      IsMerged pr ->
        case pr.vcs of
          GitHub -> evalGitHubCondition cond
          GitLab -> error "REST Condition Bool: GitLab not supported"
      HasGreenCI pr ->
        case pr.vcs of
          GitHub -> evalGitHubCondition cond
          GitLab -> error "REST Condition Bool: GitLab not supported"

-- | Throw a 'SpremutaException' if the status is not 200. Handle some codes
-- in a special way.
handleStatus :: (Show a, Monad m) => String -> C.Request -> C.Response a -> m ()
handleStatus what req response =
  case C.getResponseStatusCode response of
    200 -> return ()
    403 -> throw $ Response404 req response ("Access forbidden to " <> what)
    404 -> throw $ Response404 req response (what <> " not found ")
    _   -> throw $ ResponseKO req response

evalGitHubCondition :: MonadIO m => Condition -> m Bool
evalGitHubCondition =
  \case
    TrueCond -> return True
    IsMerged pr -> liftIO $ go IsMergedKind pr
    HasGreenCI pr -> liftIO $ go HasGreenCIKind pr
  where
    -- Can't make this MonadIO m => ... -> m Bool
    -- without introducing a newtype
    -- (see https://stackoverflow.com/questions/41751854/why-am-i-getting-overlapping-instances-error-when-one-doesnt-match)
    go :: ConditionKind -> PR -> IO Bool
    go _ck pr = do
      request <- getPR pr
      response :: C.Response Request.GetPRBody <- C.httpJSON request
      handleStatus ("PR " <> show pr) request response
      undefined
      -- let body :: Request.GetPRBody = C.getResponseBody response
      -- print body
      -- return True

-- instance (Monad m, MonadIO m) => MonadThrow m where
--   throwM = liftIO . throwIO
