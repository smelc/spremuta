module Exception where

import           Control.Exception
import qualified Network.HTTP.Simple as C

data SpremutaException where
    Response404 :: Show a => C.Request -> C.Response a -> String -> SpremutaException -- ^ A request that failed with 404. First String is a custom error message.
    Response403 :: Show a => C.Request -> C.Response a -> String -> SpremutaException -- ^ A request that failed with 403. First String is a custom error message.
    ResponseKO :: Show a => C.Request -> C.Response a -> SpremutaException -- ^ A request that failed with a code we don't handle in a special way.

instance Show SpremutaException where
  show _ = error "TODO"

instance Exception SpremutaException
