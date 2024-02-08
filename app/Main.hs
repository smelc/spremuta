module Main where

import Control.Exception (catch)
import qualified Daemon
import Exception (SpremutaException)
import qualified Options
import qualified Options.Applicative as O
import qualified Request
import System.Exit (die)
import Types

main :: IO ()
main = do
  opts@Options {command} <- O.execParser Options.optsParser
  case command of
    TaskCmd t -> do
      _ :: Request.EvalResult <- Request.eval (opts, t) `catch` handler
      return ()
    DaemonCmd frequency tasksFile ->
      Daemon.run $ Daemon.Data {frequency, tasksFile, tick = 0, options = opts}

handler :: SpremutaException -> IO a
handler e = die $ "💣 " ++ show e
