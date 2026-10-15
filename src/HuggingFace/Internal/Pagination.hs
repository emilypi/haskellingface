{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Pagination
-- Description : Private continuation representation and Link header grammar
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Pagination
    ( NextPage (..), links
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAscii, isAlphaNum, ord, toLower)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P
import HuggingFace.Internal.Utils (matchedBytes)

-- | Opaque server-provided pagination continuation.
newtype NextPage = NextPage ByteString
    deriving (Eq)

instance Show NextPage where
    show _ = "<NextPage>"

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
