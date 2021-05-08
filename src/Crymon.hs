{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

-- |
-- Copyright: (c) 2021 Fabien Boucher
-- SPDX-License-Identifier: MIT
-- Maintainer: Fabien Boucher <fabien.dot.boucher@gmail.com>
--
-- A tool to compute asset value from exchanges
module Crymon
  ( someFunc,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import qualified Data.ByteString.Base64 as B64
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Client
  ( Manager,
    Request (method, requestHeaders),
    Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

data InnerStatus = InnerStatus
  { status :: Text,
    timestamp :: UTCTime
  }
  deriving (Show)

instance FromJSON InnerStatus where
  parseJSON = withObject "InnerStatus" $ \v ->
    InnerStatus
      <$> v .: "status"
      <*> v .: "timestamp"

data SystemStatusResponse = SystemStatusResponse
  { error :: [Text],
    result :: InnerStatus
  }
  deriving (Show)

instance FromJSON SystemStatusResponse where
  parseJSON = withObject "SystemStatusResponse" $ \v ->
    SystemStatusResponse
      <$> v .: "error"
      <*> v .: "result"

getSystemStatus :: Manager -> IO (Maybe SystemStatusResponse)
getSystemStatus manager = do
  initReq <- parseRequest "https://api.kraken.com/0/public/SystemStatus"
  let request = initReq {method = "GET", requestHeaders = [("User-agent", "Crymon")]}
  response <- httpLbs request manager
  pure (decode (responseBody response) :: Maybe SystemStatusResponse)

getNonce :: IO Int64
getNonce = do
  u <- getCurrentTime
  pure (floor . (1e9 *) . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds u)

getSignedRequest :: Text
getSignedRequest = sigdigest
  where
    postDataAsQS = "nonce=1616492376594&ordertype=limit&pair=XBTUSD&price=37500&type=buy&volume=1.25"
    nonce = "1616492376594"
    encoded = nonce <> postDataAsQS
    privK = "kQH5HW/8p1uGOVjbgWA7FunAmGO8lsSUXNsu3eow76sz84Q18fWxnyRzBHCd3pd5nE9qa99HAZtuZuj6F1huXg=="
    url = "/0/private/AddOrder" :: ByteString
    message = url <> SHA256.hash encoded
    macdigest = SHA512.hmac (B64.decodeLenient privK) message
    sigdigest = decodeUtf8 . B64.encode $ macdigest

createAuthHeader :: [(Text, Text)]
createAuthHeader = [("Api-Key", "")]

someFunc :: IO ()
someFunc = do
  manager <- newManager tlsManagerSettings
  systemStatus <- getSystemStatus manager
  print systemStatus
