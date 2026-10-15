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
    , encodeSegments
    ) where

import Control.Exception (catch)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii, isAlphaNum, ord, toLower)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client hiding (TooManyRedirects)
import Network.HTTP.Types
import Network.URI (URI (..), URIAuth (..), parseURIReference, relativeTo)
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P
import HuggingFace.Internal.Types
import HuggingFace.Types (HuggingFaceError (..), Page (..))

encodeSegments :: [Text] -> ByteString
encodeSegments parts = "/" <> BS.intercalate "/" (map (urlEncode True . encodeUtf8) parts)

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

decodeJSON :: FromJSON a => BL.ByteString -> Either HuggingFaceError a
decodeJSON = either (const (Left DecodeError)) Right . eitherDecode

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

readAtMost :: Int -> BodyReader -> IO ByteString
readAtMost limit reader = BS.concat . reverse <$> go limit []
  where
    go n chunks
        | n <= 0 = pure chunks
        | otherwise = do
            chunk <- brRead reader
            if BS.null chunk then pure chunks
                else go (n - BS.length chunk) (BS.take n chunk : chunks)

sameOrigin :: Request -> Request -> Bool
sameOrigin a b = secure a == secure b && port a == port b
    && B8.map toLower (host a) == B8.map toLower (host b)

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

-- A Link header is a comma-separated list of targets and parameters. Parsing
-- quoted strings as a unit prevents embedded commas, semicolons, and escaped
-- quotes from being mistaken for delimiters or relation parameters.
links :: ByteString -> [ByteString]
links = fromRight [] . P.parse (linkHeader <* P.eof) ""

linkHeader :: P.Parser [ByteString]
linkHeader = do
    ows
    -- HTTP list recipients ignore empty members (RFC 9110, section 5.6.1.2).
    entries <- P.optionMaybe link `P.sepBy1` (P.char ',' *> ows)
    pure [target | (target, parameters) <- catMaybes entries, maybe False (elem "next" . B8.words) (lookup "rel" parameters)]
  where
    link = do
        target <- P.between (P.char '<') (P.char '>') $
            matchedBytes (P.skipMany (P.satisfy (\c -> c > ' ' && c < '\DEL' && c /= '<' && c /= '>')))
        ows
        parameters <- P.many (P.char ';' *> ows *> parameter <* ows)
        pure (target, parameters)
    parameter = do
        name <- token <* ows
        value <- P.option "" (P.char '=' *> ows *> (quoted P.<|> token))
        pure (B8.map toLower name, value)
    token = matchedBytes (P.skipMany1 (P.satisfy tokenChar))
    tokenChar c = (isAscii c && isAlphaNum c) || B8.elem c "!#$%&'*+-.^_`|~"
    quoted = P.between (P.char '"') (P.char '"') (BS.concat <$> P.many chunk)
    chunk = (P.char '\\' *> (B8.singleton <$> P.satisfy quotedPair))
        P.<|> matchedBytes (P.skipMany1 (P.satisfy quotedText))
    quotedPair c = c == '\t' || (ord c >= 32 && c /= '\DEL')
    quotedText c = quotedPair c && c /= '"' && c /= '\\'

ows :: P.Parser ()
ows = P.skipMany (P.satisfy (\c -> c == ' ' || c == '\t'))

matchedBytes :: P.Parser a -> P.Parser ByteString
matchedBytes parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (BS.take (BS.length before - BS.length after) before)
