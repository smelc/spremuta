{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Log
import Numeric.Natural

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String
  deriving (Show)

-- | Enumeration of the supported version control systems
data VCS
  = GitHub
  | GitLab
  deriving (Eq, Show, Bounded, Enum)

-- | All supported version control systems
vcss :: [VCS]
vcss = [minBound .. maxBound]

-- | A task: a single line in a spremuta.tasks file
data Task = Task Todo Condition

-- | A line coming from the tasks file
newtype TaskString = TaskString String

instance Show Task where
  show (Task todo cond) =
    show todo ++ when ++ fromMaybe "" suffix
    where
      when =
        case suffix of
          Nothing -> ""
          Just _ -> " when "
      suffix =
        case cond of
          TrueCond -> Nothing
          HasCIFinished {} -> Just $ show cond
          IsMerged {} -> Just $ show cond
          HasGreenCI {} -> Just $ show cond

-- | An effect that spremuta does on the world
data Todo
  = -- | The task to merge a PR, for example "merge https://github.com/smelc/spremuta/pull/12"
    Merge PR
  | -- | Notify the user of something: "notify"
    Notify
  | -- | The task to undraft/set ready a PR, for example "setready https://github.com/smelc/spremuta/pull/12"
    SetReady PR

instance Show Todo where
  show =
    \case
      Merge pr -> "merge " ++ show pr
      Notify -> "notify"
      SetReady pr -> "setready " ++ show pr

-- data TodoKind =
--     MergeKind
--   | SetReadyKind
--   deriving (Show, Bounded, Enum)

-- allTodoKinds :: [TodoKind]
-- allTodoKinds = [minBound .. maxBound]

-- | Some Boolean condition
data Condition
  = TrueCond
  | HasCIFinished PR
  | IsMerged PR
  | HasGreenCI PR

-- | The kind of a @Task@'s underlying condition, if any.
toConditionKind :: Task -> Maybe ConditionKind
toConditionKind =
  \case
    Task _todo TrueCond -> Nothing
    Task _todo (HasCIFinished _) -> Just HasCIFinishedKind
    Task _todo (IsMerged _) -> Just IsMergedKind
    Task _todo (HasGreenCI _) -> Just HasGreenCIKind

-- | A kind for the cases of @Condition@ where the kind is useful
data ConditionKind = HasCIFinishedKind | IsMergedKind | HasGreenCIKind
  deriving (Eq)

instance Show Condition where
  show =
    \case
      TrueCond -> "true"
      HasCIFinished pr -> show pr ++ " hascifinished"
      IsMerged pr -> show pr ++ " ismerged"
      HasGreenCI pr -> show pr ++ " hasgreenci"

-- * Types used when parting the CLI

-- | The type of options. If adding new options,
-- you probably want to extend this datatype. It's important that the
-- options that are common to all commands come first in this datatype.
-- It simplifies building the parser (see 'programOptions' below)
data Options = Options
  { -- | Note that 'logLevel' is unused in the code, because we implement
    -- verbosity levels in a hacky way in 'Request'. The parser in this file
    -- is only to document the flags to the user.
    logLevel :: Log.LogLevel,
    -- | The command to call when notifying the user
    notifyCmd :: Maybe [String],
    command :: Command
  }
  deriving (Show)

data Command
  = -- | The tasks to execute
    TaskCmd Task
  | -- | The first parameter is the frequency at which the daemon wakes up, in minutes.
    -- The second parameter is the path to file from which to read tasks.
    DaemonCmd Natural FilePath
  deriving (Show)

-- * VCS agnostic types

--
-- Types used in all backends

-- | Components that matter to call the REST API on the pull request
-- URL provided by the user. This type is VCS-agnostic for now
-- but will maybe need to be generalized when we support GitLab.
data PR = PR
  { owner :: String,
    repo :: String,
    number :: Int,
    vcs :: VCS
  }

instance Show PR where
  show PR {..} =
    case vcs of
      GitHub -> owner ++ "/" ++ repo ++ "/pull/" ++ show number
      GitLab -> error "Unsupported case GitLab in Show PR instance"

-- | Components that matter to call the REST API on a reference.
-- This type is VCS-agnostic for now
-- but will maybe need to be generalized when we support GitLab.
data CheckRuns = CheckRuns
  { owner :: String,
    repo :: String,
    sha :: String,
    vcs :: VCS
  }

-- * GitHub types

--
-- Types that are specific to GitHub

-- | The type obtained after deserializing a request to
-- https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28
data GitHubPR = GitHubPR
  { head :: GitHubRev,
    url :: String,
    merged :: Bool,
    number :: Int,
    state :: String
  }
  deriving (Generic, Show)

instance ToJSON GitHubPR

instance FromJSON GitHubPR

data GitHubRev = GitHubRev
  { sha :: String,
    repo :: GitHubRepo
  }
  deriving (Generic, Show)

instance ToJSON GitHubRev

instance FromJSON GitHubRev

-- | The type obtained after deserializing a request to
-- https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28
data GitHubRepo = GitHubRepo
  { -- | "main"
    default_branch :: String,
    -- | For example "https://github.com/IntersectMBO"
    html_url :: String,
    owner :: GitHubOwner,
    -- | The name of the repo, e.g. "cardano-cli"
    name :: String
  }
  deriving (Generic, Show)

instance ToJSON GitHubRepo

instance FromJSON GitHubRepo

data GitHubOwner = GitHubOwner
  { -- | The name of the repo, e.g. "cardano-cli"
    login :: String,
    -- | For example "https://github.com/IntersectMBO"
    html_url :: String
  }
  deriving (Generic, Show)

instance ToJSON GitHubOwner

instance FromJSON GitHubOwner

-- | Datatype modeling the answer of
-- https://docs.github.com/en/rest/checks/runs?apiVersion=2022-11-28#list-check-runs-for-a-git-reference
data GitHubCheckRuns = GitHubCheckRuns
  { check_runs :: [GitHubCheckRun]
  }
  deriving (Generic, Show)

instance ToJSON GitHubCheckRuns

instance FromJSON GitHubCheckRuns

-- | Datatype modeling part of the answer of
-- https://docs.github.com/en/rest/checks/runs?apiVersion=2022-11-28#list-check-runs-for-a-git-reference
data GitHubCheckRun = GitHubCheckRun
  { -- | The name of the run , for example "Lint"
    name :: String,
    -- | For example @"success"@, or @"failure"@. Can also be @null@
    conclusion :: Maybe String,
    -- | For example "completed"
    status :: String
  }
  deriving (Generic, Show)

instance ToJSON GitHubCheckRun

instance FromJSON GitHubCheckRun
