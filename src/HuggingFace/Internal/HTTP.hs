{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.HTTP
-- Description : HTTP transport and credential-safe redirects
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.HTTP
    ( requestFor, getJSON, postJSON, getPage, continuePage, withBody
    ) where

import Control.Exception (catch)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Network.HTTP.Client hiding (TooManyRedirects)
import Network.HTTP.Types
import Network.URI (URI (..), URIAuth (..), parseURIReference, relativeTo)
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Internal.Auth (Token (..))
import HuggingFace.Internal.Client (Client (..))
import HuggingFace.Internal.HTTP.Utils (encodeSegments, readAtMost, sameOrigin)
import HuggingFace.Internal.Pagination (NextPage (..), links)
import HuggingFace.Internal.Utils (decodeJSON)
import HuggingFace.Types.Pagination (Page (..))

requestFor :: Client -> Request -> [Text] -> Query -> Request
requestFor client base segments query = base
    { path = encodeSegments segments
    , queryString = renderQuery True query
    , redirectCount = 0
    , checkResponse = \_ _ -> pure ()
    , responseTimeout = responseTimeoutMicro (clientTimeout client)
    , cookieJar = Nothing
    , requestHeaders = [(hAccept, "application/json"), (hUserAgent, "huggingface-haskell/0.1.0.0")]
        ++ case clientToken client of
            Nothing -> []
            Just (Token token) -> [(hAuthorization, "Bearer " <> token)]
    }

getJSON :: FromJSON a => Client -> Request -> IO (Either HuggingFaceError a)
getJSON client request = fmap (>>= \(_, _, body) -> decodeJSON body) (getBytes client request)

postJSON :: (ToJSON a, FromJSON b) => Client -> Request -> a -> IO (Either HuggingFaceError b)
postJSON client request payload = getJSON client request
    { method = methodPost
    , requestBody = RequestBodyLBS (encode payload)
    , requestHeaders = (hContentType, "application/json") : requestHeaders request
    }

getBytes :: Client -> Request -> IO (Either HuggingFaceError (Request, ResponseHeaders, BL.ByteString))
getBytes client request = withBody False client request $ \response -> do
    chunks <- brConsume (responseBody response)
    pure (getOriginalRequest response, responseHeaders response, BL.fromChunks chunks)

getPage :: FromJSON a => Client -> Request -> IO (Either HuggingFaceError (Page a))
getPage client request = do
    result <- getBytes client request
    pure $ do
        (finalRequest, headers, body) <- result
        items <- decodeJSON body
        pure (Page items (continuation finalRequest headers))

continuePage :: FromJSON a => Client -> NextPage -> IO (Either HuggingFaceError (Page a))
continuePage client (NextPage url) = case redirectTarget False (clientHub client) url of
    Right target | sameOrigin target (clientHub client) ->
        let request = requestFor client target [] []
        in getPage client request {path = path target, queryString = queryString target}
    _ -> pure (Left InvalidPagination)

-- The response and its socket are closed before following another redirect.
-- Only GET redirects are followed; replaying a billable POST is never implicit.
withBody :: Bool -> Client -> Request -> (Response BodyReader -> IO a) -> IO (Either HuggingFaceError a)
withBody allowExternal client initial consume = go (5 :: Int) initial
  where
    go remaining request = do
        outcome <- catchHTTP $ withResponse request (clientManager client) $ \response -> do
            let code = statusCode (responseStatus response)
            if code >= 200 && code < 300
                then Right . Right <$> consume response
                else if method request == methodGet && code `elem` [301, 302, 303, 307, 308]
                    then pure $ if remaining <= 0 then Left TooManyRedirects else
                        case lookup hLocation (responseHeaders response) of
                            Nothing -> Left UnsafeRedirect
                            Just location -> Left <$> redirectTarget allowExternal request location
                    else Left <$> responseError response
        case outcome of
            Left err -> pure (Left err)
            Right (Right value) -> pure (Right value)
            Right (Left target) -> go (remaining - 1) target

catchHTTP :: IO (Either HuggingFaceError a) -> IO (Either HuggingFaceError a)
catchHTTP action = action `catch` handler
  where
    handler :: HttpException -> IO (Either HuggingFaceError a)
    handler (HttpExceptionRequest _ ResponseTimeout) = pure (Left (TransportError "Response timeout"))
    handler (HttpExceptionRequest _ ConnectionTimeout) = pure (Left (TransportError "Connection timeout"))
    handler _ = pure (Left (TransportError "HTTP transport failure"))

responseError :: Response BodyReader -> IO HuggingFaceError
responseError response = do
    body <- readAtMost 65536 (responseBody response)
    let headers = responseHeaders response
    pure HttpError
        { errorStatus = statusCode (responseStatus response)
        , errorBody = body
        , errorRequestId = lookup "x-request-id" headers
        , errorRetryAfter = lookup "retry-after" headers
        }

redirectTarget :: Bool -> Request -> ByteString -> Either HuggingFaceError Request
redirectTarget allowExternal current location = do
    reference <- maybe (Left UnsafeRedirect) Right (parseURIReference (B8.unpack location))
    let uri = reference `relativeTo` getUri current
    case uriAuthority uri of
        Just auth | null (uriUserInfo auth) && uriScheme uri `elem` ["http:", "https:"] -> pure ()
        _ -> Left UnsafeRedirect
    target <- maybe (Left UnsafeRedirect) Right (parseRequest (show uri) :: Maybe Request)
    if (secure current && not (secure target)) || (not allowExternal && not (sameOrigin current target))
        then Left UnsafeRedirect
        else Right target
            { method = methodGet
            , redirectCount = 0
            , checkResponse = \_ _ -> pure ()
            , responseTimeout = responseTimeout current
            , cookieJar = Nothing
            , requestHeaders = if sameOrigin current target then requestHeaders current
                else filter (\(key, _) -> key /= hAuthorization && key /= hCookie) (requestHeaders current)
            }

continuation :: Request -> ResponseHeaders -> Maybe NextPage
continuation request headers = do
    link <- listToMaybe (concatMap (links . snd) (filter ((== "link") . fst) headers))
    let resolved = case parseURIReference (B8.unpack link) of
            Just uri -> B8.pack (show (uri `relativeTo` getUri request))
            Nothing -> link
    pure (NextPage resolved)
