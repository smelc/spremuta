-- | This module is meant to be used qualified
module Parse where
import           Types

import           Control.Monad       (when)
import           Data.Function       ((&))
import           Data.List.Extra     (splitOn, stripInfix)
import           Text.Read           (readMaybe)

url :: PRURL ->  Either String PRBits
url (PRURL url) = do
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
