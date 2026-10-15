-- |
-- Module      : HuggingFace.Error
-- Description : Client errors and redacted diagnostics
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Error
    ( HuggingFaceError (..)
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Client failures. HTTP response bodies and headers are retained for callers
-- but omitted from 'Show', since servers can echo credentials or private data.
-- Transport exceptions are classified without retaining their credential-bearing
-- request. Asynchronous and non-HTTP callback exceptions propagate; HTTP
-- exceptions raised while reading a download become 'TransportError'.
data HuggingFaceError
    = InvalidInput Text
    | TransportError Text
    | HttpError
        { errorStatus :: Int
        , errorBody :: ByteString -- ^ At most 64 KiB of response content; treat as sensitive.
        , errorRequestId :: Maybe ByteString
        , errorRetryAfter :: Maybe ByteString
        }
    | DecodeError -- ^ A successful response did not match its expected JSON schema.
    | UnsafeRedirect -- ^ Cross-origin API redirect, invalid location, or HTTPS downgrade.
    | TooManyRedirects -- ^ More than five redirects.
    | InvalidPagination -- ^ A continuation does not point at the configured Hub origin.
    deriving (Eq)

instance Show HuggingFaceError where
    show (InvalidInput message) = "InvalidInput " ++ show message
    show (TransportError message) = "TransportError " ++ show message
    show HttpError {errorStatus = code} = "HttpError " ++ show code
    show DecodeError = "DecodeError"
    show UnsafeRedirect = "UnsafeRedirect"
    show TooManyRedirects = "TooManyRedirects"
    show InvalidPagination = "InvalidPagination"
