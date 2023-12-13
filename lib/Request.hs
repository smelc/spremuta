{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | How to build requests to the GitHub API.
-- This module is meant to be used qualified.
module Request (getPR, GetPRBody(..), REST(..)) where

import           Conduit                  (MonadThrow)
import           Control.Exception        (throw)
import           Control.Monad.Except
import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy     as LBS
import           Data.Function            ((&))
import           Data.Maybe               (fromJust)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as T
import           Exception                (SpremutaException (..))
import           GHC.Generics
import           Network.HTTP.Simple      (Request, addRequestHeader,
                                           parseRequest)
import qualified Network.HTTP.Simple      as C
import           Types                    (Condition (..), ConditionKind (..),
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

-- | Throws a 'SpremutaException' if the status is not 200. Handle some codes
-- in a special way.
handleStatus :: (Show a, Monad m) => C.Request -> C.Response a -> m ()
handleStatus req response =
  case C.getResponseStatusCode response of
    200 -> return ()
    403 -> throw $ Request403 req
    404 -> throw $ Request404 req
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
      response :: C.Response LBS.ByteString <- C.httpLBS request
      handleStatus request response
      let bodyBS :: LBS.ByteString = C.getResponseBody response
          errOrBody:: Either String Request.GetPRBody = Aeson.eitherDecode bodyBS
          body = case errOrBody of Left err -> throw (BadBody request err); Right b -> b
          value :: Aeson.Value = seq body (Aeson.decode bodyBS) & fromJust
      print body
      liftIO $ putStrLn $ T.unpack $ T.decodeUtf8 $ Aeson.encodePretty value
      return True
