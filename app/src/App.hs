{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (runApp, twitchClientId) where

import Data.Aeson (FromJSON (parseJSON), decode, encode)
import Data.Aeson.Types (Value, parseMaybe)
import Data.ByteString (isPrefixOf, stripPrefix)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import qualified GHC.IO
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (NoReqBody), POST (POST), defaultHttpConfig, https, jsonResponse, oAuth2Bearer, responseBody, runReq, (/:), (=:))
import qualified Network.HTTP.Req as HTTP
import Network.HTTP.Types (found302, status200, status401, status404, status500)
import Network.URI (parseURI)
import Network.Wai (Application, pathInfo, queryString, responseBuilder, responseFile)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Util (redirect)
import Network.WebSockets (defaultConnectionOptions, sendTextData, withPingThread)
import qualified Network.WebSockets as WS
import Noita.Types (NoitaUpdate (NoitaUpdate))
import qualified RedirectState
import StreamerTokenStore (getStreamerFromToken, newTokenForStreamer)
import qualified System.Environment
import System.Timeout (timeout)
import Types (StreamerInformation, blankStreamerInformation, fromNoitaUpdate)
import Util (toBytestring)
import WandChannels (BroadcastChannel, ReceiveChannel, broadcastUpdate, getBroadcastChannel, getLastUpdate, getReceiveChannel, initStreamer, receiveUpdates)

-- On Request:
-- 1. If no stream exists yet, make OnlyWands stream
-- 2. Set up stream to send broadcast updates to Twitch

runApp :: IO ()
runApp =
  run 8080 app

app :: Application
app =
  let websocketSettings =
        defaultConnectionOptions
   in websocketsOr websocketSettings appWs appHttp

twitchClientId = GHC.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_ID"

twitchClientSecret = GHC.IO.unsafePerformIO $ System.Environment.getEnv "TWITCH_API_CLIENT_SECRET"

appHttp :: Application
appHttp req send =
  let staticFileServer = staticApp $ defaultWebAppSettings "static"
      fail404 = responseBuilder status404 [] "Not found"
      fail500 = responseBuilder status500 [] "Internal error"
   in case pathInfo req of
        [] -> send $ responseFile status200 [] "static/index.html" Nothing
        ["streamer", ""] -> send fail404
        ["streamer", streamerName] -> do
          lastUpdate <- getLastUpdate $ T.unpack streamerName
          case lastUpdate of
            Nothing -> send $ responseFile status200 [] "static/streamer-not-registered.html" Nothing
            Just _ -> send $ responseFile status200 [] "static/streamer-page.html" Nothing
        ["twitch", "redirect"] -> do
          let query = queryString req
              state = fmap unpack $ snd =<< find (\(k, _v) -> k == "state") query
              authCode = snd =<< find (\(k, _v) -> k == "code") query
          s <- RedirectState.getRedirectState
          if s == state
            then case authCode of
              Nothing -> send $ responseBuilder status401 [] "Failed to get a code from Twitch"
              Just authCode -> do
                accessToken <- getAccessToken (decodeUtf8 authCode)
                case accessToken of
                  Nothing -> send $ responseBuilder status401 [] "Failed to get a code from Twitch"
                  Just token -> do
                    streamerInfo <- validateToken (toBytestring token)
                    case streamerInfo of
                      Nothing -> send fail500
                      Just (username, userId) -> do
                        token <- newTokenForStreamer (username, userId)
                        initStreamer username
                        let uri =
                              parseURI $
                                "https://wands.halfbro.xyz/success.html?streamer="
                                  ++ username
                                  ++ "&token="
                                  ++ token
                        case redirect found302 [] =<< uri of
                          Just resp -> send resp
                          _ -> send fail500
            else send $ responseBuilder status401 [] "State variable invalid"
        ["register"] -> do
          s <- RedirectState.initState
          let urlString =
                "https://id.twitch.tv/oauth2/authorize"
                  ++ "?client_id="
                  ++ twitchClientId
                  ++ "&force_verity=true"
                  ++ "&redirect_uri=https://wands.halfbro.xyz/twitch/redirect"
                  ++ "&response_type=code"
                  ++ "&scope="
                  ++ "&state="
                  ++ s
              twitchAuth = redirect found302 [] =<< parseURI urlString
          case twitchAuth of
            Just resp -> send resp
            _ -> send fail500
        _ ->
          staticFileServer req send

data TwitchUserInfo = TwitchUserInfo
  {login :: String, user_id :: String}
  deriving (Generic, Show, Eq)

instance FromJSON TwitchUserInfo

newtype TokenInfo = TokenInfo
  { access_token :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON TokenInfo

getAccessToken :: Text -> IO (Maybe String)
getAccessToken authToken =
  runReq defaultHttpConfig $ do
    res <-
      HTTP.req
        POST
        (https "id.twitch.tv" /: "oauth2" /: "token")
        NoReqBody
        jsonResponse
        ( "client_id"
            =: twitchClientId
            <> "client_secret"
            =: twitchClientSecret
            <> "code"
            =: authToken
            <> "grant_type"
            =: ("authorization_code" :: String)
            <> "redirect_uri"
            =: ("https://wands.halfbro.xyz/twitch/redirect" :: String)
        )
    let body = (responseBody res :: Value)
        tokenInfo = (parseMaybe parseJSON body :: Maybe TokenInfo)
    case tokenInfo of
      Nothing ->
        return Nothing
      Just info ->
        return $ Just $ access_token info

validateToken :: S.ByteString -> IO (Maybe (String, String))
validateToken accessToken =
  runReq defaultHttpConfig $ do
    res <-
      HTTP.req
        GET
        (https "id.twitch.tv" /: "oauth2" /: "validate")
        NoReqBody
        jsonResponse
        (oAuth2Bearer accessToken)
    let body = (responseBody res :: Value)
        twitchInfo = (parseMaybe parseJSON body :: Maybe TwitchUserInfo)
    case twitchInfo of
      Nothing ->
        return Nothing
      Just info ->
        return $ Just (login info, user_id info)

appWs :: WS.ServerApp
appWs pendingConnection = do
  user <- shouldAccept pendingConnection
  case user of
    Rejected reason -> do
      print reason
      WS.rejectRequest pendingConnection $ S.pack reason
    Viewer receiveChan lastInfo -> do
      conn <- WS.acceptRequest pendingConnection
      withPingThread conn 15 mempty $ serveViewer conn receiveChan lastInfo
    Streamer broadcastChan -> do
      conn <- WS.acceptRequest pendingConnection
      serveStreamer conn broadcastChan

data User
  = Streamer BroadcastChannel
  | Viewer ReceiveChannel StreamerInformation
  | Rejected String

validateStreamerToken :: S.ByteString -> IO (Maybe (String, String))
validateStreamerToken = getStreamerFromToken . S.unpack

shouldAccept :: WS.PendingConnection -> IO User
shouldAccept conn = do
  let head = WS.pendingRequest conn
      path = WS.requestPath head
  case stripPrefix "/updates/" path of
    Just streamerName -> do
      let name = S.unpack streamerName
      readChannel <- getReceiveChannel name
      case readChannel of
        Nothing -> return $ Rejected $ "Streamer " ++ name ++ " hasn't registered"
        Just (chan, lastInfo) -> do
          print $ "Viewer started listening for " ++ name ++ "'s updates"
          return $ Viewer chan lastInfo
    Nothing -> do
      print path
      case stripPrefix "/token/" path of
        Nothing -> do
          return $ Rejected "Invalid websocket path"
        Just token -> do
          info <- validateStreamerToken token
          case info of
            Nothing -> return $ Rejected "Invalid token"
            Just (_streamerId, streamerName) -> do
              writeChannel <- getBroadcastChannel streamerName
              case writeChannel of
                Nothing -> return $ Rejected "Error during registration"
                Just chan -> do
                  print $ "Streamer " ++ streamerName ++ " started broadcasting"
                  return $ Streamer chan

serveViewer :: WS.Connection -> ReceiveChannel -> StreamerInformation -> IO ()
serveViewer conn receiveChannel lastInfo = do
  sendTextData conn $ encode lastInfo
  receiveUpdates
    receiveChannel
    (sendTextData conn . encode)

serveStreamer :: WS.Connection -> BroadcastChannel -> IO ()
serveStreamer conn writeChannel = do
  let loop = do
        msg <- timeout 5_000_000 $ WS.receiveData conn
        let rawUpdate :: Maybe NoitaUpdate = decode =<< msg
        let update = fromNoitaUpdate <$> rawUpdate
        case update of
          Nothing ->
            loop
          Just info -> do
            broadcastUpdate writeChannel info
            loop
  loop
