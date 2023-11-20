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

-- | A typeclass to identify the VCS we're dealing with
class VCS a where
  -- Empty

-- If you add a new tag, extend 'AllVCS' accordingly

-- | A tag to identify data that lives on GitHub
data GitHub

instance VCS GitHub

-- | A tag to identify data that lives on GitLab
data GitLab

instance VCS GitLab

data OneVCS =
  GitHub
  | GitLab
  deriving (Show, Bounded, Enum)

vcss :: [OneVCS]
vcss = [minBound .. maxBound]