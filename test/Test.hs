{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where
import           Data.Either
import qualified Parse
import           Test.Hspec
import qualified Text.Megaparsec as MP
import           Types

-- | @shouldSatisfyRight (Left _) _@ fails. @shouldSatisfyRight (Right x) f@
-- succeeds if @f x@ holds.
shouldSatisfyRight :: Show a => Show b => Either a b -> (b -> Bool) -> Expectation
shouldSatisfyRight (Left a) _ = expectationFailure $ "Expected Right, got (Left " ++ show a ++ ")"
shouldSatisfyRight (Right x) f | f x = pure ()
shouldSatisfyRight (Right x) _ = expectationFailure (show x ++ " doesn't satisfy the predicate")

runParser parser url =
  case MP.runParser parser "" url of
    Left error -> Left $ MP.errorBundlePretty error
    Right x    -> Right x

parseTasks :: Expectation
parseTasks = do
  runParser Parse.pTask "merge  https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/too_short"
    `shouldSatisfy` isLeft
  runParser Parse.pTask "wrong https://github.com/smelc/spremuta/pull/1"
    `shouldSatisfy` isLeft
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1   when  True"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 isready"
    `shouldSatisfy` isRight
  runParser Parse.pTask "merge https://github.com/smelc/spremuta/pull/1 when https://gitlab.com/tezos/tezos/-/merge_requests/10922 hasgreenci"
    `shouldSatisfy` isRight

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

main :: IO ()
main = hspec $ do
  describe "parse URL" $ do
    it "GitHub" parseGitHubURLs
    it "GitLab" parseGitLabURLs
  describe "parse tasks" $ do
    it "Tasks" parseTasks
