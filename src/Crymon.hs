{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Copyright: (c) 2021 Fabien Boucher
-- SPDX-License-Identifier: MIT
-- Maintainer: Fabien Boucher <fabien.dot.boucher@gmail.com>
--
-- A tool to compute asset value from exchanges
module Crymon (displayAssets) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import Data.Bifunctor (second)
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import Data.Time
  ( UTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client
  ( Manager,
    Request (method, requestHeaders),
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
    aBRresult :: Maybe (HM.HashMap Text Text)
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
  initReq <- parseRequest url
  nonce <- getNonce
  postData <- getPostData
  let signed = getSignedRequest (toText uriPath) (getQSString initReq (map (Data.Bifunctor.second Just) postData)) (decodeUtf8 nonce) privKey
  let request =
        initReq
          { requestHeaders =
              [ ("API-Key", encodeUtf8 apiKey),
                ("API-Sign", encodeUtf8 signed),
                ("Accept-Encoding", "gzip, deflate"),
                ("Accept", "*/*")
              ]
          }
  response <- httpLbs (urlEncodedBody postData request) manager
  -- print (responseBody response)
  pure (decode (responseBody response) :: Maybe AccountBalanceResponse)
  where
    uriPath = "/0/private/Balance"
    url = "https://api.kraken.com" <> uriPath
    getPostData :: IO [(ByteString, ByteString)]
    getPostData = do
      nonce <- getNonce
      pure [("nonce" :: ByteString, nonce)]

getNonce :: IO ByteString
getNonce = do
  u <- getCurrentTime
  pure (show (floor . (1e3 *) . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds u :: Integer))

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

data Asset = Asset
  { name :: Text,
    value :: Double
  }
  deriving (Show)

displayAssets :: IO ()
displayAssets = do
  privKeyM <- lookupEnv krakenPrivKey
  let privKey = toText $ fromMaybe (error "No " <> krakenPrivKey <> " env var provided") privKeyM
  apiKeyM <- lookupEnv krakenAPIKey
  let apiKey = toText $ fromMaybe (error "No " <> krakenAPIKey <> " env var provided") apiKeyM
  manager <- newManager tlsManagerSettings
  balanceM <- getAccountBalance manager privKey apiKey
  case balanceM of
    Just (AccountBalanceResponse _ (Just balance)) -> print (toAssetList balance)
    _ -> print ("Unable to get balance" :: [Char])
  where
    toAssetList :: HM.HashMap Text Text -> [Asset]
    toAssetList hm = HM.foldrWithKey (\k v acc -> Asset k (getDouble v) : acc) [] hm
    getDouble :: Text -> Double
    getDouble dstr = fromMaybe 0.0 (readMaybe (toString dstr) :: Maybe Double)
