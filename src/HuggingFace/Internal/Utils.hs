{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Utils
-- Description : Shared parsing, JSON, text, and numeric utilities
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Utils
    ( TextParser, parseText, parseValue, parseTextJSON, parseLabel, fromEither, decodeJSON
    , matched, matchedBytes, takeWhile1P, label, asciiAlphaNum, inRange
    ) where

import Data.Aeson (FromJSON, Value, eitherDecode, withText)
import qualified Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as PB
import HuggingFace.Error (HuggingFaceError (..))

type TextParser = P.Parsec Text ()

-- The diagnostic is supplied by the caller. Parsec errors may contain input
-- tokens, so they must never escape a parser that can receive credentials.
parseText :: e -> TextParser a -> Text -> Either e a
parseText err parser = either (const (Left err)) Right . P.parse (parser <* P.eof) ""

-- Capture consumed Text without an intermediate String or rescanning the
-- remaining input. Parsec consumes complete characters, so the byte-length
-- difference always falls on a UTF-8 boundary; takeWord8 is the safe slicer.
matched :: TextParser a -> TextParser Text
matched parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (TF.takeWord8 (fromIntegral (TF.lengthWord8 before - TF.lengthWord8 after)) before)

takeWhile1P :: (Char -> Bool) -> TextParser Text
takeWhile1P predicate = matched (P.skipMany1 (P.satisfy predicate))

asciiAlphaNum :: Char -> Bool
asciiAlphaNum c = isAscii c && isAlphaNum c

-- Open namespaces preserve spaces, Unicode, and punctuation, but must contain
-- at least one non-space character and no control characters.
label :: TextParser Text
label = do
    _ <- P.lookAhead (P.skipMany (P.satisfy isSpace) *> P.satisfy (not . isSpace))
    takeWhile1P (not . isControl)

parseTextJSON :: Text -> (Text -> Either HuggingFaceError a) -> Value -> JSON.Parser a
parseTextJSON name parse = withText (T.unpack name) (fromEither . parse)

fromEither :: Either HuggingFaceError a -> JSON.Parser a
fromEither = either (fail . show) pure

parseValue :: Text -> TextParser a -> Text -> Either HuggingFaceError a
parseValue message = parseText (InvalidInput message)

parseLabel :: Text -> Text -> Either HuggingFaceError Text
parseLabel name = parseValue (name <> " requires nonblank text without control characters") label

inRange :: Double -> Double -> Double -> Bool
inRange low high t = not (isNaN t || isInfinite t) && t >= low && t <= high

decodeJSON :: FromJSON a => BL.ByteString -> Either HuggingFaceError a
decodeJSON = either (const (Left DecodeError)) Right . eitherDecode

matchedBytes :: PB.Parser a -> PB.Parser ByteString
matchedBytes parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (BS.take (BS.length before - BS.length after) before)
