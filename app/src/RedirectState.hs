module RedirectState (initState, getRedirectState, cache) where

import Data.Cache as Cache (Cache, insert, lookup, newCache)
import GHC.IO (unsafePerformIO)
import System.RandomString
  ( Alphabet (Base58),
    StringOpts (StringOpts, alphabet, nrBytes),
    randomString,
  )

{-# NOINLINE cache #-}
cache :: Cache () String
cache = unsafePerformIO $ newCache Nothing

initState :: IO String
initState = do
  state <- randomString $ StringOpts {alphabet = Base58, nrBytes = 20}
  insert cache () state
  return state

getRedirectState :: IO (Maybe String)
getRedirectState = Cache.lookup cache ()
