{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications    #-}

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

runURLParser parser url =
  case MP.runParser parser "" url of
    Left error -> Left $ MP.errorBundlePretty error
    Right x    -> Right x

main :: IO ()
main = hspec $ do
  describe "parse URL" $ do
    it "GitHub" $ do
      runURLParser (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
        `shouldSatisfy` isRight
      runURLParser (Parse.parser @'GitHub) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
        `shouldSatisfyRight` (\bits -> bits.vcs == GitHub)
      runURLParser (Parse.parser @'GitHub) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
        `shouldSatisfy` isLeft
    it "GitLab" $ do
      runURLParser (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
        `shouldSatisfy` isRight
      runURLParser (Parse.parser @'GitLab) "https://gitlab.com/tezos/tezos/-/merge_requests/10922"
        `shouldSatisfyRight` (\bits -> bits.vcs == GitLab)
      runURLParser (Parse.parser @'GitLab) "https://github.com/tbagrel1/datasheet_aggregator_10th/pull/12"
        `shouldSatisfy` isLeft
