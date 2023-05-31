module RedirectState (initState, getRedirectState, cache) where

import Data.Cache as Cache (Cache, insert, lookup, newCache)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Text.StringRandom

{-# NOINLINE cache #-}
cache :: Cache () String
cache = unsafePerformIO $ newCache Nothing

initState :: IO String
initState = do
  state <- T.unpack <$> stringRandomIO "[a-zA-Z2-9]{16}"
  insert cache () state
  return state

getRedirectState :: IO (Maybe String)
getRedirectState = Cache.lookup cache ()
