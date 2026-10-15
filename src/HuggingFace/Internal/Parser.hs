{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Parser
-- Description : Reusable domain grammars over strict Text
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Parser
    ( name, components, providerSlug, portNumber
    ) where

import Control.Monad (void, when)
import Data.Char (isControl, isDigit, ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Text.Parsec as P
import HuggingFace.Internal.Utils (TextParser, asciiAlphaNum, matched, takeWhile1P)

-- Hub name components start/end with a word character, cannot repeat '.' or
-- '-', and cannot end in '.git'. Length is bounded while consuming input.
name :: TextParser Text
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

components :: TextParser (NonEmpty Text)
components = (:|) <$> component <*> P.many (P.char '/' *> component)
  where
    component = do
        part <- takeWhile1P (\c -> c /= '/' && c /= '\\' && not (isControl c))
        case part of
            "." -> P.parserFail "Dot segment"
            ".." -> P.parserFail "Parent segment"
            _ -> pure part

providerSlug :: TextParser Text
providerSlug = takeWhile1P (\c -> asciiAlphaNum c || c == '-')

-- Accumulate with a bounded Int; fail before a later digit could overflow.
-- Leading zeroes are allowed, but a zero port is not a usable endpoint.
portNumber :: TextParser Int
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
