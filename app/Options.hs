module Options where

import           Data.Bifunctor      (first)
import           Data.List           (intercalate)
import qualified Log
import           Options.Applicative
import qualified Options.Applicative as O
import qualified Parse
import qualified System.Info         as S
import qualified Text.Megaparsec     as MP
import           Types

{- HLINT ignore "Use newtype instead of data" -}

-- | The type of options. If adding new options,
-- you probably want to extend this datatype. It's important that the
-- options that are common to all commands come first in this datatype.
-- It simplifies building the parser (see 'programOptions' below)
data Options = Options {
    -- | Note that 'logLevel' is unused in the code, because we implement
    -- verbosity levels in a hacky way in 'Request'. The parser in this file
    -- is only to document the flags to the user.
    logLevel  :: !Log.LogLevel
  , -- | The command to call when notifying the user
    notifyCmd :: Maybe [String]
  , command   :: !Command
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
   "\"spremuta daemon\" continuously reads tasks from the spremuta.tasks file."]

-- | If adding new options, this is probably where you should modify code
programOptions :: Parser Options
programOptions =
   fmap setDefaultNotify $ Options
     <$> verbosity
     <*> optional notify
     <*> hsubparser taskCommand

setDefaultNotify :: Options -> Options
setDefaultNotify o@Options {notifyCmd} =
  case notifyCmd of
    Just _  -> o -- Leave what was set by parser
    Nothing -> o {notifyCmd = defaultNotify}
   where
     defaultNotify :: Maybe [String]
     defaultNotify =
       -- See https://hackage.haskell.org/package/base-4.19.0.0/docs/System-Info.html
       -- For possible values of S.os
       case S.os of
         "linux" -> Just ["notify-send"]
         -- To default to a command on an OS, that's where to contribute:
         -- "otherOS" -> Just ["some_command", "--with-some-flag"]
         _       -> Nothing

verbosity :: Parser Log.LogLevel
verbosity = asum [verbose, debug]
  where
    verbose =
      O.flag Log.Info Log.Verbose $ mconcat
       [O.long "verbose", O.help "Output more logs. Useful for creating detailed issues. Use --debug for even more logs."]
    debug =
      O.flag Log.Info Log.Debug $ mconcat
       [O.long "debug", O.help "Output a maximum number of logs. Useful for developers."]

notify :: Parser [String]
notify =
  fmap words $ O.strOption $ mconcat
    [O.long "notify",
     O.help "Command to execute to notify the user (split on spaces). Defaults to \"notify-send\" on Linux. On other OSes, write to stdout (feel free to enhance that by modifying 'setDefaultNotify' in Options.hs)."
    ]

taskCommand :: Mod CommandFields Command
taskCommand =
  O.command ("task" :: String)
    $ info (TaskCmd <$> argument taskReader (help "TODO")) fullDesc
  where
    taskReader :: ReadM Task =
      eitherReader (first MP.errorBundlePretty .  MP.runParser Parse.pTask "")
