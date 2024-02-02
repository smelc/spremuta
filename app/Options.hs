module Options where

import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Log
import Numeric.Natural
import Options.Applicative
import qualified Options.Applicative as O
import qualified Parse
import qualified System.Info as S
import qualified Text.Megaparsec as MP
import Text.Read
import Types hiding (Options)
import qualified Types as T

optsParser :: ParserInfo T.Options
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc programDescription)
  where
    versionOption = infoOption "0.1" (long "version" <> help "Show version")

programDescription :: String
programDescription =
  intercalate
    "\n"
    [ "spremuta runs in two modes:",
      "\"spremuta task TASK\" runs the given task and exits immediately.",
      "\"spremuta daemon\" continuously reads tasks from the spremuta.tasks text file."
    ]

-- | If adding new options, this is probably where you should modify code
programOptions :: Parser T.Options
programOptions =
  fmap setDefaultNotify $
    T.Options
      <$> verbosity
      <*> optional notify
      <*> hsubparser (taskCommand <> daemonCommand)

setDefaultNotify :: T.Options -> T.Options
setDefaultNotify o@T.Options {notifyCmd} =
  case notifyCmd of
    Just _ -> o -- Leave what was set by parser
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
        _ -> Nothing

verbosity :: Parser Log.LogLevel
verbosity = asum [verbose, debug]
  where
    verbose =
      O.flag Log.Info Log.Verbose $
        mconcat
          [O.long "verbose", O.help "Output more logs. Useful for creating detailed issues. Use --debug for even more logs."]
    debug =
      O.flag Log.Info Log.Debug $
        mconcat
          [O.long "debug", O.help "Output a maximum number of logs. Useful for developers."]

notify :: Parser [String]
notify =
  fmap words $
    O.strOption $
      mconcat
        [ O.long "notify",
          O.help "Command to execute to notify the user (split on spaces). Defaults to \"notify-send\" on Linux. On other OSes, write to stdout (feel free to enhance that by modifying 'setDefaultNotify' in Options.hs)."
        ]

taskCommand :: Mod CommandFields Command
taskCommand =
  O.command ("task" :: String) $
    info
      ( TaskCmd
          <$> ( argument
                  taskReader
                  ( mconcat
                      [ O.help $
                          unlines
                            [ "The task to execute. For example:",
                              "\"merge  https://github.com/smelc/spremuta/pull/1\"",
                              "or \"notify when https://github.com/smelc/spremuta/pull/2 hasgreenci\""
                            ],
                        O.metavar "TASK"
                      ]
                  )
              )
      )
      O.fullDesc
  where
    taskReader :: ReadM Task =
      eitherReader (first MP.errorBundlePretty . MP.runParser Parse.pTask "")

daemonCommand :: Mod CommandFields Command
daemonCommand =
  O.command ("daemon" :: String) $
    info
      ( DaemonCmd
          <$> ( option
                  freqReader
                  ( mconcat
                      [ O.metavar "FREQ",
                        O.short 'f',
                        O.long "frequency",
                        O.help "The frequency at which the daemon wakes up, in minutes",
                        O.value 1,
                        O.showDefault
                      ]
                  )
              )
          <*> ( option
                  str
                  ( mconcat
                      [ O.metavar "PATH",
                        O.short 't',
                        O.long "tasks",
                        O.help "The text file (UTF8 encoded) from which to read tasks",
                        O.value "spremuta.tasks",
                        O.showDefault
                      ]
                  )
              )
      )
      O.fullDesc
  where
    freqReader :: ReadM Natural = eitherReader freqParser
    freqParser :: String -> Either String Natural
    freqParser s =
      case readMaybe s of
        Nothing -> Left $ "Cannot parse frequency argument: " ++ s
        Just n -> Right n
