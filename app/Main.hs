module Main where

import Control.Exception (catch)
import Exception (SpremutaException)
import Options (Options (..))
import qualified Options
import qualified Options.Applicative as O
import qualified Request
import System.Exit (die)
import Types

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  Options {command} <- O.execParser Options.optsParser
  let Task _todo cond = case command of Options.TaskCmd t -> t
  result :: Bool <- Request.eval cond `catch` handler
  print result
  return ()

handler :: SpremutaException -> IO a
handler e = do
  die $ "💣 " ++ show e
