{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parse
  ( parseURL,
    pTask,
    VCSParser (..),
  )
where

import Control.Monad (unless)
import Control.Monad.Extra (void)
import Data.Char (isDigit)
import Data.Either.Extra (mapLeft)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List.Extra (isInfixOf)
import Text.Megaparsec
import Text.Megaparsec.Char
import Types
import Prelude

type Parser = Parsec String String {- error type, input type -}

class VCSParser (vcs :: VCS) where
  -- | A parser, which given a PR public URL (for example "https://github.com/input-output-hk/cardano-cli/pull/522"),
  -- returns the information regarding the different segments of the PR.
  parser :: Parser PR

instance ShowErrorComponent String where
  showErrorComponent = id

parseURL :: PRURL -> Either String PR
parseURL (PRURL url) = runParser pPR "" url & mapLeft errorBundlePretty

pPR :: Parser PR
pPR = go parsers
  where
    go = \case
      [] -> customFailure $ "Cannot parse URL. Tried all VCS: " <> show vcss
      p : rest -> try p <|> go rest
    parsers :: [Parser PR]
    parsers = map parserFor vcss
    parserFor :: VCS -> Parser PR
    parserFor = \case
      GitHub -> parser @'GitHub
      GitLab -> parser @'GitLab

pTask :: Parser Task
pTask = do
  todo <- pTodo
  condition <- end <|> cond
  return $ Task todo condition
  where
    end :: Parser Condition = do
      eof
      return TrueCond
    cond :: Parser Condition = do
      hspace1
      void $ string "when"
      hspace1
      pCondition

pTodo :: Parser Todo
pTodo = choice [pMerge, pNotify, pSetReady]
  where
    pMerge = do
      void $ string "merge"
      hspace1
      Merge <$> pPR
    pNotify = do
      void $ string "notify"
      return Notify
    pSetReady = do
      void $ string "setready"
      hspace1
      SetReady <$> pPR

pCondition :: Parser Condition
pCondition = choice [pTrue, pNotTrue]
  where
    pTrue = string "True" $> TrueCond
    pNotTrue = do
      pr <- pPR
      hspace1
      s <- string "ismerged" <|> string "hasgreenci"
      return $ case s of "ismerged" -> IsMerged pr; _ -> HasGreenCI pr

pProtocol :: Parser String
pProtocol =
  choice
    [ string "https", -- Order matters with common prefixes in choice!
      string "http",
      string "ssh"
    ]

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
    _protocol <- pProtocol -- Read "https"
    void $ string "://"
    host <- parseUntilSlashInc "host" -- Read "github.com"
    unless (host `contains` "github") $ customFailure $ "Expected github to appear in hostname, but found: \"" ++ host ++ "\""
    owner <- parseUntilSlashInc "owner" -- Read "tbagrel1"
    repo <- parseUntilSlashInc "repository name" -- Read "datasheet_aggregator_10th/"
    void "pull/"
    number <- read <$> takeWhile1P (Just "PR number") isDigit
    return $ PR owner repo number GitHub

instance VCSParser 'GitLab where
  parser = do
    -- Suppose @url@ is "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    _protocol <- pProtocol -- Read "https"
    void $ string "://"
    host <- parseUntilSlashInc "host" -- Read "gitlab.com"
    unless (host `contains` "gitlab") $ customFailure $ "Expected gitlab to appear in hostname, but found: \"" ++ host ++ "\""
    owner <- parseUntilSlashInc "owner" -- Read "tezos"
    repo <- parseUntilSlashInc "repository name" -- Read "tezos"
    void "-/merge_requests/"
    number <- read <$> takeWhile1P (Just "PR number") isDigit
    return $ PR owner repo number GitLab
