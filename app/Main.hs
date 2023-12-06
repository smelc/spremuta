module Main where

import qualified Options
import qualified Options.Applicative as O
import           System.Exit         (die)
import           Types

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  Options.T {command} <- O.execParser Options.optsParser
  let Task _todo _cond = case command of Options.TaskCmd t -> t
  undefined
  -- req <- Request.getPR bits
  -- resp :: Request.GetPRResponse <- httpJSON req <&> C.getResponseBody
  -- print resp
