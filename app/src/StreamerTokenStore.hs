module StreamerTokenStore (newTokenForStreamer, getStreamerFromToken) where

import Control.Monad (when)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import Text.StringRandom

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
  -- newToken <- take 16 <$> randomString (StringOpts {alphabet = Base58, nrBytes = 32})
  newToken <- T.unpack <$> stringRandomIO "[a-zA-Z2-9]{16}"
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
      let username = last $ parseLine line
      return $ Just username
    else return Nothing
  where
    parseLine l =
      case dropWhile (== '|') l of
        "" -> []
        s' -> w : parseLine s''
          where
            (w, s'') = break (== '|') s'
