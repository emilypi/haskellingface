{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Parameters
-- Description : Constrained counts, timeouts, and sampling parameters
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Parameters
    ( PositiveInt, mkPositiveInt, positiveIntValue, defaultTimeout
    , ChatTemperature, mkChatTemperature, chatTemperatureValue
    , TopP, mkTopP, topPValue
    , GenerationTemperature, mkGenerationTemperature, generationTemperatureValue
    ) where

import Data.Aeson
import HuggingFace.Error (HuggingFaceError (..))
import HuggingFace.Internal.Utils (fromEither, inRange)

-- | Strictly positive machine integer for counts, limits, and microseconds.
-- No 'Num' instance is supplied, so arithmetic cannot bypass construction.
newtype PositiveInt = PositiveInt Int deriving (Eq, Ord, Show)

-- | Refine an external integer into a positive count.
mkPositiveInt :: Int -> Either HuggingFaceError PositiveInt
mkPositiveInt n
    | n > 0 = Right (PositiveInt n)
    | otherwise = Left (InvalidInput "Expected a positive integer")

-- | Recover a positive integer.
positiveIntValue :: PositiveInt -> Int
positiveIntValue (PositiveInt n) = n

-- | Thirty seconds in microseconds.
defaultTimeout :: PositiveInt
defaultTimeout = PositiveInt 30000000

instance ToJSON PositiveInt where toJSON = toJSON . positiveIntValue
instance FromJSON PositiveInt where parseJSON v = parseJSON v >>= fromEither . mkPositiveInt

-- | A finite value in [0,2].
newtype ChatTemperature = ChatTemperature Double deriving (Eq, Ord, Show)

-- | Refine a number into a finite value in [0,2].
mkChatTemperature :: Double -> Either HuggingFaceError ChatTemperature
mkChatTemperature t
    | inRange 0 2 t = Right (ChatTemperature t)
    | otherwise = Left (InvalidInput "ChatTemperature requires a finite value in [0,2]")

-- | Recover the numeric value.
chatTemperatureValue :: ChatTemperature -> Double
chatTemperatureValue (ChatTemperature t) = t

instance ToJSON ChatTemperature where toJSON = toJSON . chatTemperatureValue
instance FromJSON ChatTemperature where parseJSON v = parseJSON v >>= fromEither . mkChatTemperature

-- | A finite value in (0,1].
newtype TopP = TopP Double deriving (Eq, Ord, Show)

-- | Refine a number into a finite value in (0,1].
mkTopP :: Double -> Either HuggingFaceError TopP
mkTopP t
    | inRange 0 1 t && t > 0 = Right (TopP t)
    | otherwise = Left (InvalidInput "TopP requires a finite value in (0,1]")

-- | Recover the numeric value.
topPValue :: TopP -> Double
topPValue (TopP t) = t

instance ToJSON TopP where toJSON = toJSON . topPValue
instance FromJSON TopP where parseJSON v = parseJSON v >>= fromEither . mkTopP

-- | A positive finite value.
newtype GenerationTemperature = GenerationTemperature Double deriving (Eq, Ord, Show)

-- | Refine a number into a positive finite value.
mkGenerationTemperature :: Double -> Either HuggingFaceError GenerationTemperature
mkGenerationTemperature t
    | not (isNaN t || isInfinite t) && t > 0 = Right (GenerationTemperature t)
    | otherwise = Left (InvalidInput "GenerationTemperature requires a positive finite value")

-- | Recover the numeric value.
generationTemperatureValue :: GenerationTemperature -> Double
generationTemperatureValue (GenerationTemperature t) = t

instance ToJSON GenerationTemperature where toJSON = toJSON . generationTemperatureValue
instance FromJSON GenerationTemperature where parseJSON v = parseJSON v >>= fromEither . mkGenerationTemperature
