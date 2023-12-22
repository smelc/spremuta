module Main where -- !!!module name different from filename!!! (for 'cabal test')

import           System.Exit

import           Data.String.Utils
import           System.Process.Extra (readProcessWithExitCode)
import           Test.Hspec

assertSpremutaOK :: [String] -> Expectation
assertSpremutaOK callArgs = do
  (ec, stdout, stderr) <- readProcessWithExitCode program args ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      logHandle "✅ stdout" $ strip stdout
      logHandle "❌ stderr" $ strip stderr
      expectationFailure $ "🙁 Success was expected, but command failed with exit code " ++ show ec ++ ":\n" ++ unwords (program : args)
  where
    program = "cabal"
    args = ["run", "spremuta", "--"] ++ callArgs
    logHandle name content =
      putStrLn $ case content of
        "" -> "No " ++ name ++ " ¯ \\ _ (ツ) _ / ¯"
        _  -> name ++ ": " ++ content

main :: IO ()
main = hspec $ do
  describe "test" $ do
    it "task" $
      assertSpremutaOK ["task", "notify when https://github.com/IntersectMBO/cardano-cli/pull/513 ismerged"]
