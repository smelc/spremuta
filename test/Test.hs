{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Control.Monad
import Data.Either
import GHC.IO.Exception (ExitCode (ExitSuccess))
import qualified Parse
import System.Process.Extra (readProcessWithExitCode)
import Test.Hspec
import qualified Text.Megaparsec as MP
import Types

-- | @shouldSatisfyRight (Left _) _@ fails. @shouldSatisfyRight (Right x) f@
-- succeeds if @f x@ holds.
shouldSatisfyRight :: (Show a) => (Show b) => Either a b -> (b -> Bool) -> Expectation
shouldSatisfyRight (Left a) _ = expectationFailure $ "Expected Right, got (Left " ++ show a ++ ")"
shouldSatisfyRight (Right x) f | f x = pure ()
shouldSatisfyRight (Right x) _ = expectationFailure (show x ++ " doesn't satisfy the predicate")

runParser parser url =
  case MP.runParser parser "" url of
    Left error -> Left $ MP.errorBundlePretty error
    Right x -> Right x

parseTasks :: Expectation
parseTasks = do
  runParser Parse.pTask "merge  https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/too_short"
    `shouldSatisfy` isLeft
  runParser Parse.pTask "wrong https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isLeft
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when garbage"
    `shouldSatisfy` isLeft
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1   when  True"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 hasgreenci"
    `shouldSatisfy` isRight
  runParser Parse.pTask "notify when https://github.com/smelc/spremuta/pull/2 hasgreenci"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 hasgreenci"
    `shouldSatisfyRight` (\t -> toConditionKind t == Just HasGreenCIKind)

parseGitHubURLs :: Expectation
parseGitHubURLs = do
  runParser (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    `shouldSatisfy` isRight
  runParser (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    `shouldSatisfyRight` (\bits -> bits.vcs == GitHub)
  runParser (Parse.parser @'GitHub) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfy` isLeft

parseGitLabURLs :: Expectation
parseGitLabURLs = do
  runParser (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfy` isRight
  runParser (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfyRight` (\bits -> bits.vcs == GitLab)
  runParser (Parse.parser @'GitLab) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    `shouldSatisfy` isLeft

goldenHelp ::
  -- | The path of the golden file
  String ->
  -- | The arguments to pass to spremuta
  [String] ->
  Expectation
goldenHelp goldenFile callerArgs = do
  (ec, stdout, _stderr) <- readProcessWithExitCode program args ""
  when (ec /= ExitSuccess) (expectationFailure $ unwords args <> " failed with exit code " <> show ec <> ", whereas a success was expected")
  golden <- readFile goldenFile
  stdout `shouldBe` golden
  where
    program = "cabal"
    -- --verbose=0 avoids printing "Up to date"
    args = ["--verbose=0", "run", "spremuta", "--"] ++ callerArgs ++ ["--help"]

main :: IO ()
main = hspec $ do
  describe "golden" $ do
    -- Regenerate golden file with: cabal --verbose=0 run spremuta -- --help >| test/golden/help.txt
    it "top-level help" $ goldenHelp "test/golden/help.txt" []
    -- Regenerate golden file with: cabal --verbose=0 run spremuta -- task --help >| test/golden/help_task.txt
    it "top-level help" $ goldenHelp "test/golden/help.txt" []
    -- Regenerate golden file with: cabal --verbose=0 run spremuta -- daemon --help >| test/golden/help_daemon.txt
    it "top-level help" $ goldenHelp "test/golden/help.txt" []
  describe "parse URL" $ do
    it "GitHub" parseGitHubURLs
    it "GitLab" parseGitLabURLs
  describe "parse tasks" $ do
    it "Tasks" parseTasks
