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
    , Endpoint, parseEndpoint, endpointText, defaultHubEndpoint, defaultInferenceEndpoint
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
    ( Manager, Request, defaultRequest, getUri, host, managerRetryableException
    , newManager, parseRequest, port, secure )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)
import HuggingFace.Internal.Types (Client (..))
import HuggingFace.Types

-- | Parsed HTTP(S) origin. The request representation is retained so client
-- construction never reparses URLs. Plain HTTP is restricted to loopback.
newtype Endpoint = Endpoint Request

instance Show Endpoint where
    show = show . endpointText

instance Eq Endpoint where
    a == b = endpointText a == endpointText b

-- | Parse an HTTPS origin (or loopback HTTP origin), with no credentials,
-- non-root path, query, or fragment. Parse errors never echo the input.
parseEndpoint :: Text -> Either HuggingFaceError Endpoint
parseEndpoint input = do
    uri <- maybe malformed Right (parseURI (T.unpack input))
    authority <- maybe malformed Right (uriAuthority uri)
    parsedPort <- case uriPort authority of
        "" -> Right (if uriScheme uri == "https:" then 443 else 80)
        ':' : digits -> case readMaybe digits :: Maybe Integer of
            Just number | number >= 1 && number <= 65535 -> Right (fromInteger number)
            _ -> malformed
        _ -> malformed
    if not (null (uriUserInfo authority)) || null (uriRegName authority)
        || not (null (uriQuery uri) && null (uriFragment uri))
        || uriPath uri `notElem` ["", "/"]
        then malformed
        else case uriScheme uri of
            "https:" -> request parsedPort
            "http:" | uriRegName authority `elem` ["localhost", "127.0.0.1", "[::1]"] -> request parsedPort
            _ -> malformed
  where
    request parsedPort = maybe malformed (\r -> Right (Endpoint r {port = parsedPort}))
        (parseRequest (T.unpack input) :: Maybe Request)
    malformed = Left (InvalidInput "Endpoint requires an HTTPS origin, or loopback HTTP origin, with a port in 1–65535 and no credentials, path, query, or fragment")

-- | Render the parsed origin. A missing root slash is normalized to @/@.
endpointText :: Endpoint -> Text
endpointText (Endpoint request) = T.pack (show (getUri request))

-- | Public Hub origin.
defaultHubEndpoint :: Endpoint
defaultHubEndpoint = Endpoint defaultRequest {secure = True, host = "huggingface.co", port = 443}

-- | Public Inference Providers router origin.
defaultInferenceEndpoint :: Endpoint
defaultInferenceEndpoint = Endpoint defaultRequest {secure = True, host = "router.huggingface.co", port = 443}

-- | Trusted client configuration built from parsed endpoints and a positive
-- timeout. The same token is sent to both origins: only configure endpoints
-- that you trust with it.
data ClientConfig = ClientConfig
    { hubEndpoint :: Endpoint -- ^ Default: 'defaultHubEndpoint'.
    , inferenceEndpoint :: Endpoint -- ^ Default: 'defaultInferenceEndpoint'.
    , accessToken :: Maybe Token -- ^ Optional for public Hub reads.
    , timeoutMicroseconds :: PositiveInt -- ^ Positive timeout, default thirty seconds.
    } deriving (Show)

-- | Public Hugging Face endpoints with no token and a thirty-second timeout.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig defaultHubEndpoint defaultInferenceEndpoint Nothing defaultTimeout

-- | Create a reusable TLS connection pool. Configuration is already parsed;
-- constructing the client does not perform any network requests.
newClient :: ClientConfig -> IO Client
newClient config = do
    manager <- newManager tlsManagerSettings {managerRetryableException = const False}
    pure (newClientWithManager manager config)

-- | Use a caller-owned manager. The caller controls its TLS, proxy, and lifetime
-- settings; request-altering manager hooks must not reintroduce credentials.
-- Set @managerRetryableException = const False@ when creating it: http-client's
-- default retry policy can replay a POST if a reused connection loses a response.
-- The no-automatic-retry guarantee requires this setting for custom managers.
newClientWithManager :: Manager -> ClientConfig -> Client
newClientWithManager manager config =
    let Endpoint hub = hubEndpoint config
        Endpoint inference = inferenceEndpoint config
    in Client manager hub inference (accessToken config) (positiveIntValue (timeoutMicroseconds config))
