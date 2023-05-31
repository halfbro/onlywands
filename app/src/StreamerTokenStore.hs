module StreamerTokenStore (newTokenForStreamer, getStreamerFromToken) where

import Control.Monad (when)
import Data.Strings (strSplit)
import System.Directory (doesFileExist, removeFile)
import System.RandomString

type Token = String

type TokenFilePath = FilePath

data Streamer = Streamer
  { username :: String,
    twitchId :: String
  }

type StreamerFilePath = FilePath

tokenFile :: Token -> String
tokenFile tokenString = "./tokens/" ++ tokenString ++ ".token"

readTokenFile :: TokenFilePath -> IO Token
readTokenFile = readFile

streamerFile :: Streamer -> String
streamerFile (Streamer name _) = "./tokens/" ++ name ++ ".user"

readStreamerFile :: TokenFilePath -> IO Token
readStreamerFile = readFile

revokeTokenForStreamer :: Streamer -> IO ()
revokeTokenForStreamer streamer = do
  let streamerFilePath = streamerFile streamer
  exists <- doesFileExist streamerFilePath

  when
    exists
    ( do
        token <- readTokenFile streamerFilePath
        let tokenFilePath = tokenFile token
        removeFile streamerFilePath
        removeFile tokenFilePath
    )

newTokenForStreamer :: String -> String -> IO Token
newTokenForStreamer username twitchId = do
  let streamer = Streamer username twitchId
  revokeTokenForStreamer streamer
  newToken <- take 16 <$> randomString (StringOpts {alphabet = Base58, nrBytes = 32})
  writeFile (tokenFile newToken) (twitchId ++ "|" ++ username)
  writeFile (streamerFile streamer) newToken
  putStrLn $ "Created new token for user '" ++ username ++ "'"
  return newToken

getStreamerFromToken :: String -> IO (Maybe String)
getStreamerFromToken t = do
  let path = tokenFile t
  exists <- doesFileExist path
  if exists
    then do
      line <- readFile path
      let (_, username) = strSplit "|" line
      return $ Just username
    else return Nothing
