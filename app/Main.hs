module Main where

import           Data.Functor
import           Network.HTTP.Simple (httpJSON)
import qualified Network.HTTP.Simple as C
import qualified Parse
import qualified Request
import           System.Environment  (getArgs)
import           System.Exit         (die)
import           Types               (PRURL (..))

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  args <- getArgs
  prURL <- case args of
      (url:_) -> return $ PRURL url
      []      -> emojiDie "Please provide a pull request URL as an argument."
  bits <- case Parse.prURL prURL of
    Right bits -> return bits
    Left err   -> emojiDie err
  putStr $ show bits
  req <- Request.getPR bits
  resp :: Request.GetPRResponse <- httpJSON req <&> C.getResponseBody
  print resp
