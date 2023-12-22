module Main where

import Control.Exception (catch)
import Exception (SpremutaException)
import qualified Options
import qualified Options.Applicative as O
import qualified Request
import System.Exit (die)
import Types

main :: IO ()
main = do
  opts@Options {command} <- O.execParser Options.optsParser
  let t = case command of TaskCmd t -> t
  Request.eval (opts, t) `catch` handler

handler :: SpremutaException -> IO a
handler e = do
  die $ "💣 " ++ show e
