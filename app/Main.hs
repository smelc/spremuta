module Main where

import qualified Parse
import           System.Environment (getArgs)
import           System.Exit        (die)
import           Types              (PRURL (..))

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  args <- getArgs
  prURL <- case args of
      (url:_) -> return $ PRURL url
      []      -> emojiDie "Please provide a pull request URL as an argument."
  bits <- case Parse.url prURL of
    Right bits -> return bits
    Left err   -> emojiDie err
  putStr $ show bits

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
