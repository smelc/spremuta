module Types where
import           Data.Maybe (fromMaybe)

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String
  deriving (Show)

-- | Components that matter to call the GitHub API on the pull request
-- URL provided by the user.
data PR = PR {
  owner  :: String,
  repo   :: String,
  number :: Int,
  vcs    :: VCS
} deriving (Show)

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
      Notify -> "notifyme"
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

-- | A kind for the cases of @Condition@ where the kind is useful
data ConditionKind = IsMergedKind | HasGreenCIKind

instance Show Condition where
  show =
    \case
      TrueCond -> "true"
      IsMerged pr -> show pr ++ " ismerged"
      HasGreenCI pr -> show pr ++ " hasgreenci"
