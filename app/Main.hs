module Main where

import           Control.Monad       (when)
import           Data.Aeson
import           Data.Function       ((&))
import           Data.List.Extra     (splitOn, stripInfix)
import           Network.HTTP.Simple
import           System.Environment  (getArgs)
import           System.Exit         (die)
import           Text.Read           (readMaybe)

newtype PRURL = PRURL String

-- | Components that matter to call the GitHub API on the pull request
-- URL provided by the user.
data PRBits = PRBits {
  owner  :: String,
  repo   :: String,
  number :: Int
} deriving (Show)

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  args <- getArgs
  prURL <- case args of
      (url:_) -> return $ PRURL url
      []      -> emojiDie "Please provide a pull request URL as an argument."
  bits <- case parse prURL of
    Right bits -> return bits
    Left err -> emojiDie err
  putStr $ show bits

parse :: PRURL ->  Either String PRBits
parse (PRURL url) = do
  -- Suppose @url@ is "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
  (_, afterProtocol) <- stripInfix "//" url & \case Nothing -> Left $ "Double slash not found in URL: " ++ url
                                                    Just x -> Right x
  -- At this point @afterProtocol@ is "github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
  let segments = splitOn "/" afterProtocol
  let nbSegments = length segments
  when (nbSegments < 5) (Left $ "URL doesn't have enough segments (after the protocol): " ++ url ++ ". Expected at least 5, but found " ++ show nbSegments)
  let owner = segments !! 1
      repo = segments !! 2
      numberStr = segments !! 4
  number <- readMaybe numberStr & \case Nothing -> Left $ "Cannot parse pull request number: " ++ numberStr
                                        Just x -> Right x
  return $ PRBits owner repo number

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
