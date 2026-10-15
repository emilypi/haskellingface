{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Tasks
-- Description : HF Inference task parameters and results
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Tasks
    ( GenerationParameters (..), defaultGenerationParameters
    , GeneratedText (..), Classification (..)
    ) where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import HuggingFace.Types.Parameters (PositiveInt, GenerationTemperature)

-- | Parameters for the HF Inference text-generation task.
data GenerationParameters = GenerationParameters
    { generationMaxNewTokens :: Maybe PositiveInt -- ^ Positive output token limit.
    , generationTemperature :: Maybe GenerationTemperature -- ^ Positive finite sampling temperature.
    , generationDoSample :: Maybe Bool -- ^ Enable sampling.
    , generationReturnFullText :: Maybe Bool -- ^ Include the prompt in output.
    } deriving (Eq, Show)

-- | Use the server's generation defaults.
defaultGenerationParameters :: GenerationParameters
defaultGenerationParameters = GenerationParameters Nothing Nothing Nothing Nothing

instance ToJSON GenerationParameters where
    toJSON parameters = object $ catMaybes
        [ ("max_new_tokens" .=) <$> generationMaxNewTokens parameters
        , ("temperature" .=) <$> generationTemperature parameters
        , ("do_sample" .=) <$> generationDoSample parameters
        , ("return_full_text" .=) <$> generationReturnFullText parameters
        ]

-- | Generated text and optional token-level details.
data GeneratedText = GeneratedText
    { generatedText :: Text -- ^ Generated output.
    , generationDetails :: Maybe Value -- ^ Provider-specific token and sampling details.
    } deriving (Eq, Show)

instance FromJSON GeneratedText where
    parseJSON = withObject "GeneratedText" $ \o -> GeneratedText <$> o .: "generated_text" <*> o .:? "details"

-- | A predicted label and score.
data Classification = Classification
    { classificationLabel :: Text -- ^ Predicted label.
    , classificationScore :: Double -- ^ Model confidence score.
    } deriving (Eq, Show)

instance FromJSON Classification where
    parseJSON = withObject "Classification" $ \o -> Classification <$> o .: "label" <*> o .: "score"
