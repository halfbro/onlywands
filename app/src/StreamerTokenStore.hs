module StreamerTokenStore (newTokenForStreamer, getStreamerFromToken) where

import System.RandomString
import System.IO (IOMode(WriteMode))
import Data.Strings (strSplit)
import System.Directory (doesFileExist)

type Token = String

type Streamer = (String, String)

tokenFile :: String -> String
tokenFile s = "./tokens/" ++ s ++ ".token"

newTokenForStreamer :: Streamer -> IO Token
newTokenForStreamer (username, userid) = do
  t <- randomString (StringOpts {alphabet = Base58, nrBytes = 20})
  writeFile (tokenFile t) (userid ++ "|" ++ username)
  return t

getStreamerFromToken :: String -> IO (Maybe Streamer)
getStreamerFromToken t = do
  let path = tokenFile t
  exists <- doesFileExist path
  if exists then do
    line <- readFile path
    return $ Just $ strSplit "|" line
  else
    return Nothing
