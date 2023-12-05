module Main where

-- import           Data.Functor
-- import           Network.HTTP.Simple (httpJSON)
-- import qualified Network.HTTP.Simple as C
import qualified Options
import qualified Options.Applicative as O
-- import qualified Parse
-- import qualified Request
-- import           System.Environment  (getArgs)
import           System.Exit         (die)
-- import           Types               (PRURL (..))

emojiDie :: String -> IO a
emojiDie msg = die $ "❌ " ++ msg

main :: IO ()
main = do
  t <- O.execParser Options.optsParser
  print t
  -- req <- Request.getPR bits
  -- resp :: Request.GetPRResponse <- httpJSON req <&> C.getResponseBody
  -- print resp
