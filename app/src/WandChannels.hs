module WandChannels
  ( broadcastUpdate,
    receiveUpdates,
    getBroadcastChannel,
    getReceiveChannel,
    getLastUpdate,
    initStreamer,
    ReceiveChannel,
    BroadcastChannel,
    lastState,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi (InChan, OutChan, dupChan, newChan, readChan, writeChan)
import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Focus (adjust)
import GHC.IO (unsafePerformIO)
import qualified StmContainers.Map as StmMap
import qualified System.Directory as Directory
import Types (StreamerInformation, blankStreamerInformation)

data StreamerChannelInfo = StreamerChannelInfo
  { writeChannel :: InChan StreamerInformation,
    lastState :: StreamerInformation
  }

newtype BroadcastChannel = BroadcastChannel (InChan StreamerInformation)

newtype ReceiveChannel = ReceiveChannel (OutChan StreamerInformation)

{-# NOINLINE activeStreams #-}
activeStreams :: StmMap.Map String StreamerChannelInfo
activeStreams = unsafePerformIO StmMap.newIO

streamerFile :: String -> FilePath
streamerFile username = "./streamers/" ++ username ++ ".info"

initStreamer :: String -> IO ()
initStreamer username = do
  B.writeFile (streamerFile username) $ encode blankStreamerInformation
  putStrLn $ "Created streamer file for user" ++ username ++ ": " ++ streamerFile username

lookupStream :: String -> IO (Maybe StreamerChannelInfo)
lookupStream username = do
  existingStream <- atomically $ StmMap.lookup username activeStreams
  case existingStream of
    Just info -> do
      return $ Just info
    Nothing -> do
      let file = streamerFile username
      exists <- Directory.doesFileExist file
      if exists
        then do
          inJson <- B.readFile file
          let lastState = decode inJson
          (inChan, outChan) <- newChan
          void $ forkIO $ writeUpdatesToFile username outChan
          let info =
                StreamerChannelInfo
                  { writeChannel = inChan,
                    lastState = fromMaybe blankStreamerInformation lastState
                  }
          atomically $ StmMap.insert info username activeStreams
          return $ Just info
        else return Nothing

writeUpdatesToFile :: String -> OutChan StreamerInformation -> IO ()
writeUpdatesToFile username outChan =
  let loop = do
        info <- readChan outChan
        B.writeFile (streamerFile username) $ encode info
        atomically $
          StmMap.focus
            (adjust (\old -> old {lastState = info}))
            username
            activeStreams
        loop
   in loop

getLastUpdate :: String -> IO (Maybe StreamerInformation)
getLastUpdate streamerName = do
  channelInfo <- lookupStream streamerName
  return $ lastState <$> channelInfo

getBroadcastChannel :: String -> IO (Maybe BroadcastChannel)
getBroadcastChannel streamerName = do
  channelInfo <- lookupStream streamerName
  return $ BroadcastChannel . writeChannel <$> channelInfo

getReceiveChannel :: String -> IO (Maybe (ReceiveChannel, StreamerInformation))
getReceiveChannel streamerName = do
  channelInfo <- lookupStream streamerName
  case channelInfo of
    Nothing -> return Nothing
    Just info -> do
      newOutChan <- dupChan $ writeChannel info
      return $ Just (ReceiveChannel newOutChan, lastState info)

receiveUpdates :: ReceiveChannel -> (StreamerInformation -> IO ()) -> IO ()
receiveUpdates (ReceiveChannel chan) f =
  let loop = do
        info <- readChan chan
        f info
        loop
   in loop

broadcastUpdate :: BroadcastChannel -> StreamerInformation -> IO ()
broadcastUpdate (BroadcastChannel chan) = writeChan chan
