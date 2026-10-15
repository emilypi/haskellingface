{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Client
-- Description : Connection pooling, authentication, and endpoint configuration
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Client
    ( Client, ClientConfig (..), defaultClientConfig, newClient, newClientWithManager
    ) where

import Network.HTTP.Client (Manager, Request, managerRetryableException, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI (..), URIAuth (..), parseURI)
import HuggingFace.Internal.Types (Client (..))
import HuggingFace.Types

-- | Trusted client configuration. Endpoints must be HTTP(S) origins without
-- paths, credentials, queries, or fragments. Plain HTTP is restricted to
-- loopback addresses for development. The same token is sent to both configured
-- origins: only configure endpoints that you trust with it.
data ClientConfig = ClientConfig
    { hubEndpoint :: String -- ^ Default: @https://huggingface.co@.
    , inferenceEndpoint :: String -- ^ Default: @https://router.huggingface.co@.
    , accessToken :: Maybe Token -- ^ Optional for public Hub reads.
    , timeoutMicroseconds :: Int -- ^ Positive response timeout, default 30 seconds.
    } deriving (Show)

-- | Public Hugging Face endpoints with no token and a 30-second timeout.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig "https://huggingface.co" "https://router.huggingface.co" Nothing 30000000

-- | Create a reusable client with a TLS-enabled connection pool.
newClient :: ClientConfig -> IO (Either HuggingFaceError Client)
newClient config = case validateConfig config of
    Left err -> pure (Left err)
    Right _ -> do
        manager <- newManager tlsManagerSettings {managerRetryableException = const False}
        pure (newClientWithManager manager config)

-- | Use a caller-owned manager. The caller controls its TLS, proxy, and lifetime
-- settings; request-altering manager hooks must not reintroduce credentials.
-- Set @managerRetryableException = const False@ when creating it: http-client's
-- default retry policy can replay a POST if a reused connection loses a response.
-- The no-automatic-retry guarantee requires this setting for custom managers.
newClientWithManager :: Manager -> ClientConfig -> Either HuggingFaceError Client
newClientWithManager manager config = do
    (hub, inference) <- validateConfig config
    pure (Client manager hub inference (accessToken config) (timeoutMicroseconds config))

validateConfig :: ClientConfig -> Either HuggingFaceError (Request, Request)
validateConfig config
    | timeoutMicroseconds config <= 0 = Left (InvalidInput "Response timeout must be positive")
    | otherwise = (,) <$> endpoint (hubEndpoint config) <*> endpoint (inferenceEndpoint config)
  where
    endpoint url = case (parseURI url, parseRequest url :: Maybe Request) of
        (Just uri, Just request) | validOrigin uri -> Right request
        _ -> Left (InvalidInput "Endpoint must be an HTTPS origin, or a loopback HTTP origin, without credentials, path, query, or fragment")
    validOrigin uri = null (uriQuery uri) && null (uriFragment uri)
        && uriPath uri `elem` ["", "/"] && case uriAuthority uri of
            Just auth -> null (uriUserInfo auth) && not (null (uriRegName auth))
                && (uriScheme uri == "https:" || (uriScheme uri == "http:"
                    && uriRegName auth `elem` ["localhost", "127.0.0.1", "[::1]"]))
            Nothing -> False
