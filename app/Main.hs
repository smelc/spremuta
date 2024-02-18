module Main where

import Control.Exception (catch)
import Control.Monad.IO.Class
import qualified Daemon
import Exception (SpremutaException)
import Log (MonadLogger (..))
import qualified Options
import qualified Options.Applicative as O
import qualified Request
import System.Environment (lookupEnv)
import System.Exit (die)
import Types
import Prelude hiding (log)

main :: IO ()
main = do
  options@Options {command} <- O.execParser Options.optsParser
  auth <- getAuth
  case command of
    TaskCmd task -> do
      let input = Request.RestInput {auth, options, task}
      _ :: Request.EvalResult <- Request.eval input `catch` handler
      return ()
    DaemonCmd frequency tasksFile ->
      Daemon.run $ Daemon.Data {auth, frequency, tasksFile, tick = 0, options}

handler :: SpremutaException -> IO a
handler e = die $ "💣 " ++ show e

getAuth :: (MonadIO m, MonadLogger m) => m (Maybe VCSAuth)
getAuth = do
  user <- liftIO $ lookupEnv gh_user
  pat <- liftIO $ lookupEnv gh_pat
  logPickup gh_user user
  logPickup gh_pat pat
  case (user, pat) of
    (Just user, Just pat) -> return $ Just $ VCSAuth {user, pat}
    _ -> return Nothing
  where
    gh_user = "SPREMUTA_GITHUB_USER"
    gh_pat = "SPREMUTA_GITHUB_PAT"
    logPickup var value =
      case value of
        Nothing -> log $ "Cannot authenticate to GitHub, because " <> var <> " is not defined"
        Just _ -> verbose $ "Picking up " <> var <> " 👍"
