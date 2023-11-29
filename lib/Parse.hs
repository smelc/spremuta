{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is meant to be used qualified
module Parse (
  prURL,
  VCSParser(..)
  )
  where
import           Types

import           Prelude

import           Control.Monad        (unless)
import           Control.Monad.Extra  (void)
import           Data.Char            (isDigit)
import           Data.Either.Extra    (mapLeft)
import           Data.Function        ((&))
import           Data.List.Extra      (isInfixOf)
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- HLINT ignore "Use section" -}

type Parser = Parsec String String {- error type, input type -}

class VCSParser (vcs :: VCS) where
  parser :: Parser PRBits

instance ShowErrorComponent String where
  showErrorComponent = id

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
    parseOne vcs (PRURL url)=
      runParser
        (case vcs of
          GitHub -> parser @'GitHub
          GitLab -> parser @'GitLab)
        ""
        url
        & mapLeft errorBundlePretty

_pKind :: Parser TodoKind
_pKind = choice
  [ MergeKind <$ string "merge"
  , SetReadyKind <$ string "setready" ]

pProtocol :: Parser String
pProtocol = choice
  [ string "https" -- Order matters with common prefixes in choice!
  , string "http"
  , string "ssh" ]

-- | Parses a segment of a URL, which is not the last, because it must
-- end with a slash. The argument indicates what should be parsed (for error messages)
parseUntilSlashInc :: String -> Parser String
parseUntilSlashInc label = do
  res <- takeWhile1P (Just label) ((/=) '/')
  void $ string "/"
  return res

-- | @"Foobarbidule" `contains` "bar"@ holds
contains :: String -> String -> Bool
contains = flip isInfixOf

instance VCSParser 'GitHub where
  parser = do
    -- Suppose URL is "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    _protocol <- pProtocol  -- Read "https"
    void $ string "://"
    host <- parseUntilSlashInc "host" -- Read "github.com"
    unless (host `contains` "github") $ customFailure $ "Expected github to appear in hostname, but found: \"" ++ host ++ "\""
    owner <- parseUntilSlashInc "owner" -- Read "tbagrel1"
    repo <- parseUntilSlashInc "repository name" -- Read "datasheet_aggregator_10th/"
    void "pull/"
    number <- read <$> takeWhile1P (Just "PR number") isDigit
    return $ PRBits owner repo number GitHub

instance VCSParser 'GitLab where
  parser = do
    -- Suppose @url@ is "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    _protocol <- pProtocol  -- Read "https"
    void $ string "://"
    host <- parseUntilSlashInc "host" -- Read "gitlab.com"
    unless (host `contains` "gitlab") $ customFailure $ "Expected gitlab to appear in hostname, but found: \"" ++ host ++ "\""
    owner <- parseUntilSlashInc "owner" -- Read "tezos"
    repo <- parseUntilSlashInc "repository name" -- Read "tezos"
    void "-/merge_requests/"
    number <- read <$> takeWhile1P (Just "PR number") isDigit
    return $ PRBits owner repo number GitLab
