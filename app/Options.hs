module Options where

import           Data.Bifunctor      (first)
import           Data.List           (intercalate)
import qualified Log
import           Options.Applicative
import qualified Options.Applicative as O
import qualified Parse
import qualified Text.Megaparsec     as MP
import           Types

{- HLINT ignore "Use newtype instead of data" -}

-- | The type of options. If adding new options,
-- you probably want to extend this datatype.
data Options = Options {
    command  :: !Command
  , -- | Note that 'logLevel' is unused in the code, because we implement
    -- verbosity levels in a hacky way in 'Request'. The parser in this file
    -- is only to document the flags to the user.
    logLevel :: !Log.LogLevel
} deriving Show

data Command =
  TaskCmd !Task
  deriving Show

optsParser :: ParserInfo Options
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

-- | If adding new options, this is probably where you should modify code
programOptions :: Parser Options
programOptions =
   flip Options
     <$> verbosity
     <*> hsubparser taskCommand

verbosity :: Parser Log.LogLevel
verbosity = asum [verbose, debug]
  where
    verbose =
      O.flag Log.Info Log.Verbose $ mconcat
       [O.long "verbose", O.help "Output more logs. Useful for creating detailed issues. Use --debug for even more logs."]
    debug =
      O.flag Log.Info Log.Debug $ mconcat
       [O.long "debug", O.help "Output a maximum logs. Useful for developers."]

taskCommand :: Mod CommandFields Command
taskCommand =
  O.command ("task" :: String)
    $ info (TaskCmd <$> argument taskReader (help "TODO")) fullDesc
  where
    taskReader :: ReadM Task =
      eitherReader (first MP.errorBundlePretty .  MP.runParser Parse.pTask "")
