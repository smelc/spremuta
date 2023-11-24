{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeApplications    #-}
-- | This module is meant to be used qualified
module Parse (
  prURL,
  ) where
import           Types

import           Prelude         hiding (any)

import           Control.Monad   (when)
import           Data.Function   ((&))
import           Data.List.Extra (splitOn, stripInfix)
import           Text.Read       (readMaybe)

class VCSParser (vcs :: VCS) where
  parseURL :: PRURL -> Either String PRBits

prURL :: PRURL -> Either String PRBits
prURL pr@(PRURL url) =
  go parsers
  where
    go :: [PRURL -> Either String PRBits] -> Either String PRBits
    go [] = Left $ "Cannot parse PR URL: " <> url <> ". Tried all VCS: " <> show vcss
    go (parser : rest) =
      case parser pr of
        Left _  -> go rest
        Right x -> Right x
    parsers :: [PRURL -> Either String PRBits]
    parsers = map parseOne vcss
    parseOne :: VCS -> PRURL -> Either String PRBits
    parseOne = \case GitHub -> parseURL @'GitHub; GitLab -> parseURL @'GitLab

instance VCSParser 'GitHub where
  parseURL (PRURL url) = do
    -- Suppose @url@ is "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    (_, afterProtocol) <- stripInfix "//" url & \case Nothing -> Left $ "Double slash not found in URL: " ++ url
                                                      Just x -> Right x
    -- @afterProtocol@ is "github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    let segments = splitOn "/" afterProtocol
    -- @segments@ is "["github.com","tbagrel1","datasheet_aggregator_10th","pull","12"]"
    let nbSegments = length segments
    when (nbSegments < 5) (Left $ "URL doesn't have enough segments (after the protocol): " ++ url ++ ". Expected at least 5, but found " ++ show nbSegments)
    let owner = segments !! 1
        repo = segments !! 2
        numberStr = segments !! 4
    number <- readMaybe numberStr & \case Nothing -> Left $ "Cannot parse pull request number: " ++ numberStr
                                          Just x -> Right x
    return $ PRBits owner repo number GitHub

instance VCSParser 'GitLab where
  parseURL (PRURL url) = do
    -- Suppose @url@ is "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    (_, afterProtocol) <- stripInfix "//" url & \case Nothing -> Left $ "Double slash not found in URL: " ++ url
                                                      Just x -> Right x
    -- @afterProtocol@ is "gitlab.com/tezos/tezos/-/merge_requests/10922"
    let segments = splitOn "/" afterProtocol
    -- @segments@ is @["gitlab.com","tezos","tezos","-","merge_requests","10922"]@
    let nbSegments = length segments
    when (nbSegments < 6) (Left $ "URL doesn't have enough segments (after the protocol): " ++ url ++ ". Expected at least 6, but found " ++ show nbSegments)
    let owner = segments !! 1
        repo = segments !! 2
        numberStr = segments !! 5
    number <- readMaybe numberStr & \case Nothing -> Left $ "Cannot parse pull request number: " ++ numberStr
                                          Just x -> Right x
    return $ PRBits owner repo number GitLab
