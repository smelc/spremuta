module Types where

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String
  deriving (Show)

-- | Components that matter to call the GitHub API on the pull request
-- URL provided by the user.
data PRBits = PRBits {
  owner  :: String,
  repo   :: String,
  number :: Int,
  vcs    :: VCS
} deriving (Show)

-- | Enumeration of the supported version control systems
data VCS =
  GitHub
  | GitLab
  deriving (Show, Bounded, Enum)

-- | All supported version control systems
vcss :: [VCS]
vcss = [minBound .. maxBound]

-- | A task: a single line in a spremuta.tasks file
data Task = Task Todo Condition

-- | An effect that spremuta does on the world
data Todo =
    Merge PRURL -- ^ The task to merge a PR, for example "merge https://github.com/smelc/spremuta/pull/12"
  | SetReady PRURL -- ^ The task to undraft/set ready a PR, for example "setready https://github.com/smelc/spremuta/pull/12"
  deriving (Show)

data TodoKind =
    MergeKind
  | SetReadyKind
  deriving (Show, Bounded, Enum)

allTodoKinds :: [TodoKind]
allTodoKinds = [minBound .. maxBound]

-- | Some Boolean condition
data Condition =
    TrueCond
  | IsMerged PRURL
  | HasGreenCI PRURL
  deriving (Show)
