{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import           Network.HTTP.Simple (httpJSON)
import qualified Network.HTTP.Simple as C
import qualified Parse
import qualified Request
import           System.Environment  (getArgs)
import           System.Exit         (die)
import           Types               (PRURL (..))
import Data.Functor

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

-- handleResponse :: Exception exc => Either exc a -> IO a
-- fromEither

main :: IO ()
main = do
  args <- getArgs
  prURL <- case args of
      (url:_) -> return $ PRURL url
      []      -> emojiDie "Please provide a pull request URL as an argument."
  bits <- case Parse.any prURL of
    Right bits -> return bits
    Left err   -> emojiDie err
  putStr $ show bits
  req <- Request.getPR bits
  resp :: Request.GetPRResponse <- httpJSON req <&> C.getResponseBody
  print resp
  undefined

-- getPullRequest :: PullRequestUrl -> IO ()
-- getPullRequest url = do
--   request <- parseRequest url
--   response <- httpJSON request
--   let isDraft = response & getResponseBody
--   let statusChecksPass = undefined -- You'll need to fill this in based on the GitHub API
--   undefined

-- markReady :: PullRequestUrl -> IO ()
-- markReady url = do
--    request <- parseRequest ("POST " ++ url ++ "/ready_for_review")
--    _ <- httpNoBody request
--    return ()
