{-# LANGUAGE ScopedTypeVariables #-}

module Secrets (getTwitchClientId, getTwitchClientSecret) where

import Control.Exception (assert, throw)
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import GHC.Generics
import Network.HTTP.Req
import System.Environment (getEnv)

checkNonEmpty :: String -> Maybe String
checkNonEmpty "" = Nothing
checkNonEmpty s = Just s

data GoogleAccessTokenResponse = GoogleAccessTokenResponse
  { access_token :: String,
    expires_in :: Integer,
    token_type :: String
  }
  deriving (Generic)

instance ToJSON GoogleAccessTokenResponse

instance FromJSON GoogleAccessTokenResponse

data GoogleSecretResponsePayload = GoogleSecretResponsePayload
  { _data :: String,
    dataCrc32c :: String
  }
  deriving (Generic)

instance ToJSON GoogleSecretResponsePayload

instance FromJSON GoogleSecretResponsePayload

data GoogleSecretResponse = GoogleSecretResponse
  { name :: String,
    payload :: GoogleSecretResponsePayload
  }
  deriving (Generic)

instance ToJSON GoogleSecretResponse

instance FromJSON GoogleSecretResponse

getComputeServiceAccountToken :: IO (Maybe GoogleAccessTokenResponse)
getComputeServiceAccountToken = do
  let accessTokenUri =
        http "metadata.google.internal"
          /: "computeMetadata"
          /: "v1"
          /: "instance"
          /: "service-accounts"
          /: "default"
          /: "token"
  let req' = req GET accessTokenUri NoReqBody jsonResponse (header "Metadata-Flavor" "Google")
  accessTokenResponse :: Value <- responseBody <$> runReq defaultHttpConfig req'
  return $ parseMaybe parseJSON accessTokenResponse

getGoogleSecret :: String -> IO (Maybe String)
getGoogleSecret secret = do
  maybeAccessToken <- getComputeServiceAccountToken
  case maybeAccessToken of
    Nothing ->
      return Nothing
    Just GoogleAccessTokenResponse {access_token = accessToken} -> do
      let uri =
            https "secretmanager.googleapis.com"
              /: "v1"
              /: "projects"
              /: "onlywands"
              /: "secrets"
              /: T.pack secret
              /: "versions"
              /: "latest:access"
          headers = oAuth2Bearer (BS.pack accessToken) <> header "Content-Type" "application/json"
      response :: Value <- responseBody <$> runReq defaultHttpConfig (req GET uri NoReqBody jsonResponse headers)
      let secret :: Maybe GoogleSecretResponse = parseMaybe parseJSON response
      return $ fmap (_data . payload) secret

shouldUseLocalEnv :: IO Bool
shouldUseLocalEnv = do
  env <- getEnv "USE_LOCAL_ENVIRONMENT"
  case env of
    "1" -> return True
    "true" -> return True
    _ -> return False

getTwitchClientId :: IO String
getTwitchClientId = do
  useLocalEnv <- shouldUseLocalEnv
  if useLocalEnv
    then do
      tid <- checkNonEmpty <$> getEnv "TWITCH_API_CLIENT_ID"
      return $ fromMaybe (error "Couldn't get Twitch client Id from Environment") tid
    else do
      tid <- getGoogleSecret "twitch-api-client-id"
      return $ fromMaybe (error "Couldn't get Twitch client Id from Google Secret Manager") tid

getTwitchClientSecret :: IO String
getTwitchClientSecret = do
  useLocalEnv <- shouldUseLocalEnv
  if useLocalEnv
    then do
      tsecret <- checkNonEmpty <$> getEnv "TWITCH_API_CLIENT_SECRET"
      return $ fromMaybe (error "Couldn't get Twitch client secret from Environment") tsecret
    else do
      tsecret <- getGoogleSecret "twitch-api-client-secret"
      return $ fromMaybe (error "Couldn't get Twitch client secret from Google Secret Manager") tsecret
