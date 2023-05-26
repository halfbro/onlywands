module StreamerTokenStore (newTokenForStreamer, getStreamerFromToken) where

import Data.Strings (strSplit)
import System.Directory (doesFileExist)
import System.IO (IOMode (WriteMode))
import System.RandomString

type Token = String

type Streamer = (String, String)

tokenFile :: String -> String
tokenFile s = "./tokens/" ++ s ++ ".token"

newTokenForStreamer :: Streamer -> IO Token
newTokenForStreamer (username, userid) = do
  t <- take 16 <$> randomString (StringOpts {alphabet = Base58, nrBytes = 32})
  writeFile (tokenFile t) (userid ++ "|" ++ username)
  putStrLn $ "Created token file for user '" ++ username ++ "': " ++ tokenFile t
  return t

getStreamerFromToken :: String -> IO (Maybe Streamer)
getStreamerFromToken t = do
  let path = tokenFile t
  exists <- doesFileExist path
  if exists
    then do
      line <- readFile path
      return $ Just $ strSplit "|" line
    else return Nothing
