{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String
  deriving (Show)

-- | Enumeration of the supported version control systems
data VCS =
  GitHub
  | GitLab
  deriving (Eq, Show, Bounded, Enum)

-- | All supported version control systems
vcss :: [VCS]
vcss = [minBound .. maxBound]

-- | A task: a single line in a spremuta.tasks file
data Task = Task Todo Condition

instance Show Task where
  show (Task todo cond) =
    show todo ++ when ++ fromMaybe  "" suffix
    where
      when =
        case suffix of
          Nothing -> ""
          Just _  -> " when "
      suffix =
        case cond of
          TrueCond      -> Nothing
          IsMerged {}   -> Just $ show cond
          HasGreenCI {} -> Just $ show cond



-- | An effect that spremuta does on the world
data Todo =
    Merge PR -- ^ The task to merge a PR, for example "merge https://github.com/smelc/spremuta/pull/12"
  | Notify -- ^ Notify the user of something: "notify"
  | SetReady PR -- ^ The task to undraft/set ready a PR, for example "setready https://github.com/smelc/spremuta/pull/12"

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
data Condition =
    TrueCond
  | IsMerged PR
  | HasGreenCI PR

-- | The kind of a @Task@'s underlying condition, if any.
toConditionKind :: Task -> Maybe ConditionKind
toConditionKind =
  \case Task _todo TrueCond -> Nothing
        Task _todo (IsMerged _) -> Just IsMergedKind
        Task _todo (HasGreenCI _) -> Just HasGreenCIKind

-- | A kind for the cases of @Condition@ where the kind is useful
data ConditionKind = IsMergedKind | HasGreenCIKind
  deriving Eq

instance Show Condition where
  show =
    \case
      TrueCond -> "true"
      IsMerged pr -> show pr ++ " ismerged"
      HasGreenCI pr -> show pr ++ " hasgreenci"

-- * VCS agnostic types
--
-- Types used in all backends

-- | Components that matter to call the REST API on the pull request
-- URL provided by the user. This type is VCS-agnostic.
data PR = PR {
  owner  :: String,
  repo   :: String,
  number :: Int,
  vcs    :: VCS
} deriving (Show)

-- * GitHub types
--
-- Types that are specific to GitHub

-- | The type obtained after deserializing a request to
-- https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28
data GitHubPR = GitHubPR {
    url    :: String
  , merged :: Bool
  , number :: Int
  , state  :: String
} deriving (Generic, Show)

instance ToJSON GitHubPR where

instance FromJSON GitHubPR where

-- | The type obtained after deserializing a request to
-- https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28
data GitHubRepo = GitHubRepo {
    url   :: String -- | The URL of the repo, in REST terms, e.g. "https://api.github.com/repos/input-output-hk/cardano-cli"
    -- | The owner of the repo, e.g. "input-output-hk"
  , login :: String
    -- | The name of the repo, e.g. "cardano-cli"
  , name  :: String
} deriving (Generic, Show)

instance ToJSON GitHubRepo where

instance FromJSON GitHubRepo where
