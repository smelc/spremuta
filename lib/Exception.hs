module Exception where

import Control.Exception
import Log
import qualified Network.HTTP.Simple as C

newtype SuggestDebug = SuggestDebug Bool

data SpremutaException where
  -- | Body of request could not be parsed. 'String' is the error message from Aeson.
  -- Use 'mkBadBody' to build values.
  BadBody :: C.Request -> String -> SuggestDebug -> SpremutaException
  -- | A request that failed with 404.
  Request404 :: C.Request -> SpremutaException
  -- | A request that failed with 403.
  Request403 :: C.Request -> SpremutaException
  -- | A request that failed with a code we don't handle in a special way.
  ResponseKO :: (Show a) => C.Request -> C.Response a -> SpremutaException

-- | Smart constructor for @BadBody@
mkBadBody :: (MonadLogger m) => C.Request -> String -> m SpremutaException
mkBadBody r s = do
  hasDebug <- hasDebug
  return $ BadBody r s (SuggestDebug (not hasDebug))

instance Show SpremutaException where
  show =
    \case
      BadBody req err (SuggestDebug suggestDebug) ->
        "When performing request: "
          <> show req
          <> "Could not parse body: "
          <> err
          <> " 🙁"
          <> (if suggestDebug then " Pass --debug to get more details." else "")
      Request404 req -> go req 404
      Request403 req -> go req 403
      ResponseKO req response -> go2 req response
    where
      go :: C.Request -> Int -> String
      go req code =
        "When performing request: "
          <> show req
          <> "received code "
          <> show code
          <> ", whereas 200 (OK) was expected 🙁"
      go2 :: (Show a) => C.Request -> C.Response a -> String
      go2 (req :: C.Request) (response :: C.Response a) =
        "When performing request: "
          <> show req
          <> "got response:"
          <> show response
          <> "\n"
          <> "and code is "
          <> show (C.getResponseStatusCode response)
          <> ", whereas 200 (OK) was expected 🙁"

instance Exception SpremutaException
