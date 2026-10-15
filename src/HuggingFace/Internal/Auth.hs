{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Auth
-- Description : Private bearer-token representation and parser
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Auth
    ( Token (..), parseToken
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import HuggingFace.Error (HuggingFaceError)
import HuggingFace.Internal.Utils (parseValue, takeWhile1P, asciiAlphaNum)

-- | Opaque bearer token with a redacted 'Show' instance.
newtype Token = Token ByteString

instance Show Token where
    show _ = "<Token>"

-- | Parse a bearer token without retaining malformed input in errors.
-- Its 'Show' instance never reveals the value.
parseToken :: Text -> Either HuggingFaceError Token
parseToken = parseValue "Token requires ASCII letters, digits, underscores, or hyphens" $
    Token . encodeUtf8 <$> takeWhile1P (\c -> asciiAlphaNum c || c == '_' || c == '-')
