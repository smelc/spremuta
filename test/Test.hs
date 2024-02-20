{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Control.Monad
import Data.Either
import GHC.IO.Exception (ExitCode (ExitSuccess))
import qualified Parse
import System.Process.Extra (readProcessWithExitCode)
import qualified TasksFile
import Test.Hspec
import Types

-- | @shouldSatisfyRight (Left _) _@ fails. @shouldSatisfyRight (Right x) f@
-- succeeds if @f x@ holds.
shouldSatisfyRight :: (Show a) => (Show b) => Either a b -> (b -> Bool) -> Expectation
shouldSatisfyRight (Left a) _ = expectationFailure $ "Expected Right, got (Left " ++ show a ++ ")"
shouldSatisfyRight (Right x) f | f x = pure ()
shouldSatisfyRight (Right x) _ = expectationFailure (show x ++ " doesn't satisfy the predicate")

parseTasks :: Expectation
parseTasks = do
  Parse.parseAny Parse.pTask "merge  https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isRight
  Parse.parseAny Parse.pTask "merge https://github.com/too_short"
    `shouldSatisfy` isLeft
  Parse.parseAny Parse.pTask "wrong https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isLeft
  Parse.parseAny Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when garbage"
    `shouldSatisfy` isLeft
  Parse.parseAny Parse.pTask "merge https://github.com/smelc/spremuta/pull/1   when  True"
    `shouldSatisfy` isRight
  Parse.parseAny Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 hasgreenci"
    `shouldSatisfy` isRight
  Parse.parseAny Parse.pTask "notify when https://github.com/smelc/spremuta/pull/2 hasgreenci"
    `shouldSatisfy` isRight
  Parse.parseAny Parse.pTask "notify when https://github.com/smelc/spremuta/pull/2 hascifinished"
    `shouldSatisfy` isRight
  Parse.parseAny Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 hasgreenci"
    `shouldSatisfyRight` (\t -> toConditionKind t == Just HasGreenCIKind)
  Parse.parseAny Parse.pTask "notify when https://github.com/smelc/spremuta/pull/2 hascifinished"
    `shouldSatisfyRight` (\t -> toConditionKind t == Just HasCIFinishedKind)

parseGitHubURLs :: Expectation
parseGitHubURLs = do
  Parse.parseAny (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    `shouldSatisfy` isRight
  Parse.parseAny (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
    `shouldSatisfyRight` (\bits -> bits.vcs == GitHub)
  Parse.parseAny (Parse.parser @'GitHub) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfy` isLeft

parseGitLabURLs :: Expectation
parseGitLabURLs = do
  Parse.parseAny (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfy` isRight
  Parse.parseAny (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
    `shouldSatisfyRight` (\bits -> bits.vcs == GitLab)
  Parse.parseAny (Parse.parser @'GitLab) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
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

commentTask :: Expectation
commentTask = do
  TasksFile.mapTask' f (mkTestFileOps []) () (TaskString "")
    `shouldReturn` TasksFile.Nop
  TasksFile.mapTask' f (mkTestFileOps ["foo"]) () (TaskString "foo")
    `shouldReturn` (TasksFile.Mapped 1)
  TasksFile.mapTask' f (mkTestFileOps ["foo"]) () (TaskString "bar")
    `shouldReturn` TasksFile.Nop
  TasksFile.mapTask' f (mkTestFileOps ["foo", "bar"]) () (TaskString "bar")
    `shouldReturn` (TasksFile.Mapped 2)
  where
    -- A mock for @TasksFile.FileOps@ that mimicks a file
    -- that exists, has the given content, and writing to the file does nothing.
    mkTestFileOps :: (Monad m) => [String] -> TasksFile.FileOps () m
    mkTestFileOps content =
      TasksFile.FileOps
        { fileExists = const $ return True,
          readFile = const $ return $ unlines content,
          writeFile = const $ return $ pure ()
        }
    f s = "# " <> s

isCommented :: Expectation
isCommented = do
  TasksFile.isCommented (TaskString "#foo") `shouldBe` True
  TasksFile.isCommented (TaskString "# foo") `shouldBe` True
  TasksFile.isCommented (TaskString "   # foo") `shouldBe` True
  TasksFile.isCommented (TaskString " blah") `shouldBe` False
  TasksFile.isCommented (TaskString " blah # foo") `shouldBe` False
  TasksFile.isCommented (TaskString "") `shouldBe` False

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
  describe "Tasks" $ do
    it "parse" parseTasks
    it "comment" commentTask -- This test shows warning of the form "This is unexpected". That's normal.
    it "isCommented" isCommented
