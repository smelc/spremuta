module Options where

import           Data.Bifunctor      (first)
import           Data.List           (intercalate)
import           Options.Applicative
import qualified Options.Applicative as O
import qualified Parse
import qualified Text.Megaparsec     as MP
import           Types

{- HLINT ignore "Use newtype instead of data" -}

-- | The type of options. To be meant to be used qualified.
data T = T {
  command :: !Command
} deriving Show

data Command =
  TaskCmd Task
  deriving Show

optsParser :: ParserInfo T
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc programDescription)
  where
    versionOption = infoOption "0.1" (long "version" <> help "Show version")

programDescription :: String
programDescription =
  intercalate "\n"
  ["spremuta runs in two modes:",
   "\"spremuta task TASK\" runs the given task and exits immediately.",
   "\"spremuta daemon\" that continuously reads tasks from the spremuta.tasks file."]

programOptions :: Parser T
programOptions =
   T <$> hsubparser taskCommand

taskCommand :: Mod CommandFields Command
taskCommand =
  O.command ("task" :: String)
    $ info (TaskCmd <$> argument taskReader (help "TODO")) fullDesc
  where
    taskReader :: ReadM Task =
      eitherReader (first MP.errorBundlePretty .  MP.runParser Parse.pTask "")

