module Types where

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String

-- | Components that matter to call the GitHub API on the pull request
-- URL provided by the user.
data PRBits = PRBits {
  owner  :: String,
  repo   :: String,
  number :: Int
} deriving (Show)

data VCSKind =
  GitHub
  | GitLab
  deriving (Show, Bounded, Enum)

vcss :: [VCSKind]
vcss = [minBound .. maxBound]