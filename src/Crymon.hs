{-# LANGUAGE FlexibleInstances #-}
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
import Data.Aeson (FromJSON, Object, decode, encode, parseJSON, withObject, (.:))
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Client
  ( Manager,
    Request (method, requestBody, requestHeaders),
    Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
    queryString,
    responseBody,
    setQueryString,
    urlEncodedBody,
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
  { sSRerror :: [Text],
    sSRresult :: InnerStatus
  }
  deriving (Show)

instance FromJSON SystemStatusResponse where
  parseJSON = withObject "SystemStatusResponse" $ \v ->
    SystemStatusResponse
      <$> v .: "error"
      <*> v .: "result"

data AccountBalanceResponse = AccountBalanceResponse
  { aBRerror :: [Text],
    aBRresult :: Object
  }
  deriving (Show)

instance FromJSON AccountBalanceResponse where
  parseJSON = withObject "AccountBalanceResponse" $ \v ->
    AccountBalanceResponse
      <$> v .: "error"
      <*> v .: "result"

getSystemStatus :: Manager -> IO (Maybe SystemStatusResponse)
getSystemStatus manager = do
  initReq <- parseRequest "https://api.kraken.com/0/public/SystemStatus"
  let request = initReq {method = "GET", requestHeaders = [("User-agent", "Crymon")]}
  response <- httpLbs request manager
  pure (decode (responseBody response) :: Maybe SystemStatusResponse)

getAccountBalance :: Manager -> Text -> Text -> IO (Maybe AccountBalanceResponse)
getAccountBalance manager privKey apiKey = do
  let url = "https://api.kraken.com/0/private/Balance"
  initReq <- parseRequest url
  nonce <- getNonce
  let postdata = [("nonce" :: ByteString, nonce)]
  let signed = getSignedRequest "/0/private/Balance" (getQSString initReq (map (\t -> (fst t, Just (snd t))) postdata)) (decodeUtf8 nonce) privKey
  let request =
        initReq
          { requestHeaders =
              [ ("API-Key", encodeUtf8 apiKey),
                ("API-Sign", encodeUtf8 signed),
                ("Accept-Encoding", "gzip, deflate"),
                ("Accept", "*/*")
              ]
          }
  let r = urlEncodedBody postdata request
  response <- httpLbs r manager
  print (responseBody response)
  pure (decode (responseBody response) :: Maybe AccountBalanceResponse)

-- pure Nothing

getNonce :: IO ByteString
getNonce = do
  u <- getCurrentTime
  pure (show (floor . (1e3 *) . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds u))

getQSString :: Request -> [(ByteString, Maybe ByteString)] -> Text
getQSString req attributes =
  toText (drop 1 (qs :: String))
  where
    qs = decodeUtf8 (queryString $ setQueryString attributes req)

getSignedRequest :: Text -> Text -> Text -> Text -> Text
getSignedRequest url postDataQS nonce privK = sigdigest
  where
    encoded = encodeUtf8 (nonce <> postDataQS)
    message = encodeUtf8 url <> SHA256.hash encoded
    macdigest = SHA512.hmac (B64.decodeLenient $ encodeUtf8 privK) message
    sigdigest = decodeUtf8 . B64.encode $ macdigest

-- Use demo data to test the getSignRequest function
-- https://docs.kraken.com/rest/#section/Authentication/Headers-and-Signature
testSign :: Bool
testSign = signDigest == getSignedRequest uriPath encodedPayload nonce privKey
  where
    uriPath = "/0/private/AddOrder"
    encodedPayload = "nonce=1616492376594&ordertype=limit&pair=XBTUSD&price=37500&type=buy&volume=1.25"
    nonce = "1616492376594"
    privKey = "kQH5HW/8p1uGOVjbgWA7FunAmGO8lsSUXNsu3eow76sz84Q18fWxnyRzBHCd3pd5nE9qa99HAZtuZuj6F1huXg=="
    signDigest = "4/dpxb3iT4tp/ZCVEwSnEsLxx0bqyhLpdfOpc6fn7OR8+UClSV5n9E6aSS8MPtnRfp32bAb0nmbRn6H8ndwLUQ=="

krakenPrivKey :: String
krakenPrivKey = "KRAKEN_PRIV_KEY"

krakenAPIKey :: String
krakenAPIKey = "KRAKEN_API_KEY"

someFunc :: IO ()
someFunc = do
  privKeyM <- lookupEnv krakenPrivKey
  let privKey = toText $ fromMaybe (error "No " <> krakenPrivKey <> " env var provided") privKeyM
  apiKeyM <- lookupEnv krakenAPIKey
  let apiKey = toText $ fromMaybe (error "No " <> krakenAPIKey <> " env var provided") apiKeyM
  manager <- newManager tlsManagerSettings
  Just balance <- getAccountBalance manager privKey apiKey
  let hm = aBRresult balance
  let Just value = HM.lookup "DOT" hm
  print (decode $ encode value :: Maybe String)
