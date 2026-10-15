{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Parser
-- Description : Shared Parsec grammars over strict Text
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Parser
    ( Parser, parseText, matched, takeWhile1P
    , asciiAlphaNum, name, components, label, providerSlug, portNumber
    ) where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isAscii, isControl, isDigit, isSpace, ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

-- The diagnostic is supplied by the caller. Parsec errors may contain input
-- tokens, so they must never escape a parser that can receive credentials.
parseText :: e -> Parser a -> Text -> Either e a
parseText err parser = either (const (Left err)) Right . P.parse (parser <* P.eof) ""

-- Capture consumed Text without an intermediate String or rescanning the
-- remaining input. Parsec consumes complete characters, so the byte-length
-- difference always falls on a UTF-8 boundary; takeWord8 is the safe slicer.
matched :: Parser a -> Parser Text
matched parser = do
    before <- P.getInput
    _ <- parser
    after <- P.getInput
    pure (TF.takeWord8 (fromIntegral (TF.lengthWord8 before - TF.lengthWord8 after)) before)

takeWhile1P :: (Char -> Bool) -> Parser Text
takeWhile1P predicate = matched (P.skipMany1 (P.satisfy predicate))

asciiAlphaNum :: Char -> Bool
asciiAlphaNum c = isAscii c && isAlphaNum c

-- Hub name components start/end with a word character, cannot repeat '.' or
-- '-', and cannot end in '.git'. Length is bounded while consuming input.
name :: Parser Text
name = matched $ word *> rest (95 :: Int)
  where
    word = P.satisfy (\c -> asciiAlphaNum c || c == '_')
    nameChar = P.satisfy (\c -> asciiAlphaNum c || c == '_' || c == '-' || c == '.')
    next = void word P.<|> do
        P.notFollowedBy (P.try (P.string ".git" <* P.notFollowedBy nameChar))
        punctuation <- P.char '-' P.<|> P.char '.'
        P.notFollowedBy (P.char punctuation)
        void (P.lookAhead nameChar)
    rest 0 = P.notFollowedBy nameChar
    rest n = P.optional (next *> rest (n - 1))

components :: Parser (NonEmpty Text)
components = (:|) <$> component <*> P.many (P.char '/' *> component)
  where
    component = do
        part <- takeWhile1P (\c -> c /= '/' && c /= '\\' && not (isControl c))
        case part of
            "." -> P.parserFail "Dot segment"
            ".." -> P.parserFail "Parent segment"
            _ -> pure part

-- Open namespaces preserve spaces, Unicode, and punctuation, but must contain
-- at least one non-space character and no control characters.
label :: Parser Text
label = do
    _ <- P.lookAhead (P.skipMany (P.satisfy isSpace) *> P.satisfy (not . isSpace))
    takeWhile1P (not . isControl)

providerSlug :: Parser Text
providerSlug = takeWhile1P (\c -> asciiAlphaNum c || c == '-')

-- Accumulate with a bounded Int; fail before a later digit could overflow.
-- Leading zeroes are allowed, but a zero port is not a usable endpoint.
portNumber :: Parser Int
portNumber = do
    first <- digit
    number <- rest first
    when (number == 0) (P.parserFail "Zero port")
    pure number
  where
    digit = (\c -> ord c - ord '0') <$> P.satisfy isDigit
    rest n = P.option n $ do
        d <- digit
        let next = 10 * n + d
        when (next > 65535) (P.parserFail "Port out of range")
        rest next
