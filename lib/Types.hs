module Types where

-- | A newtype wrapping the string provided by users
newtype PRURL = PRURL String

-- | Components that matter to call the GitHub API on the pull request
-- URL provided by the user.
data PRBits = PRBits {
  owner  :: String,
  repo   :: String,
  number :: Int,
  vcs :: VCS
} deriving (Show)

data VCS =
  GitHub
  | GitLab
  deriving (Show, Bounded, Enum)

vcss :: [VCS]
vcss = [minBound .. maxBound]