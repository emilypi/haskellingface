{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Inference
-- Description : Hosted chat completions and HF Inference tasks
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Inference
    ( ChatMessage (..), userMessage, systemMessage, assistantMessage
    , ChatRequest (..), defaultChatRequest, ChatCompletion (..), ChatChoice (..), Usage (..)
    , chatCompletion
    , GenerationParameters (..), defaultGenerationParameters, GeneratedText (..), textGeneration
    , Classification (..), classifyText, featureExtraction
    , infer
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import HuggingFace.Internal.HTTP
import HuggingFace.Internal.Types (Client (..))
import HuggingFace.Types

-- | A chat message. JSON content supports text, multimodal arrays, and null
-- content for tool calls. Provider-specific tool payloads are preserved as JSON.
data ChatMessage = ChatMessage
    { messageRole :: Text -- ^ For example @system@, @user@, @assistant@, or @tool@.
    , messageContent :: Maybe Value -- ^ Text or multimodal content; absent for some tool calls.
    , messageToolCalls :: Maybe Value -- ^ Tool call array, when present.
    , messageToolCallId :: Maybe Text -- ^ Identifier for a tool response.
    , messageName :: Maybe Text -- ^ Optional participant name.
    } deriving (Eq, Show)

instance ToJSON ChatMessage where
    toJSON message = object $ ("role" .= messageRole message)
        : catMaybes
            [ ("content" .=) <$> messageContent message
            , ("tool_calls" .=) <$> messageToolCalls message
            , ("tool_call_id" .=) <$> messageToolCallId message
            , ("name" .=) <$> messageName message
            ]

instance FromJSON ChatMessage where
    parseJSON = withObject "ChatMessage" $ \o -> ChatMessage
        <$> o .: "role" <*> o .:? "content" <*> o .:? "tool_calls"
        <*> o .:? "tool_call_id" <*> o .:? "name"

-- | Construct a plain-text user message.
userMessage :: Text -> ChatMessage
userMessage = textMessage "user"

-- | Construct a plain-text system message.
systemMessage :: Text -> ChatMessage
systemMessage = textMessage "system"

-- | Construct a plain-text assistant message.
assistantMessage :: Text -> ChatMessage
assistantMessage = textMessage "assistant"

textMessage :: Text -> Text -> ChatMessage
textMessage role content = ChatMessage role (Just (String content)) Nothing Nothing Nothing

-- | Non-streaming chat request for the router's OpenAI-compatible endpoint.
-- Model names can include a provider suffix such as @:fastest@ or @:together@.
data ChatRequest = ChatRequest
    { chatModel :: Text -- ^ Hub model identifier and optional routing suffix.
    , chatMessages :: [ChatMessage] -- ^ A nonempty conversation.
    , chatMaxTokens :: Maybe Int -- ^ Positive output token limit.
    , chatTemperature :: Maybe Double -- ^ Finite sampling temperature in @[0,2]@.
    , chatTopP :: Maybe Double -- ^ Finite probability in @(0,1]@.
    , chatExtra :: Object -- ^ Additional fields, e.g. @tools@ or @response_format@.
    } deriving (Eq, Show)

-- | Construct a request with server-default sampling settings.
defaultChatRequest :: Text -> [ChatMessage] -> ChatRequest
defaultChatRequest model messages = ChatRequest model messages Nothing Nothing Nothing KM.empty

instance ToJSON ChatRequest where
    toJSON request = Object $ KM.union fields extras
      where
        fields = KM.fromList $ ["model" .= chatModel request, "messages" .= chatMessages request, "stream" .= False]
            ++ catMaybes
                [ ("max_tokens" .=) <$> chatMaxTokens request
                , ("temperature" .=) <$> chatTemperature request
                , ("top_p" .=) <$> chatTopP request
                ]
        -- Reserved fields cannot bypass validation or enable streaming.
        extras = foldr KM.delete (chatExtra request) ["model", "messages", "stream", "max_tokens", "temperature", "top_p"]

-- | Token accounting when reported by the provider.
data Usage = Usage
    { promptTokens :: Integer -- ^ Tokens in the prompt.
    , completionTokens :: Integer -- ^ Tokens generated.
    , totalTokens :: Integer -- ^ Total billed/reported tokens.
    } deriving (Eq, Show)

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \o -> Usage
        <$> o .: "prompt_tokens" <*> o .: "completion_tokens" <*> o .: "total_tokens"

-- | One completion candidate.
data ChatChoice = ChatChoice
    { choiceIndex :: Int -- ^ Candidate index.
    , choiceMessage :: ChatMessage -- ^ Assistant message, including tool calls.
    , choiceFinishReason :: Maybe Text -- ^ Provider stop reason.
    } deriving (Eq, Show)

instance FromJSON ChatChoice where
    parseJSON = withObject "ChatChoice" $ \o -> ChatChoice
        <$> o .: "index" <*> o .: "message" <*> o .:? "finish_reason"

-- | Completed non-streaming response. Additional fields remain in 'completionRaw'.
data ChatCompletion = ChatCompletion
    { completionId :: Text -- ^ Request identifier assigned by the provider.
    , completionModel :: Text -- ^ Resolved model.
    , completionChoices :: [ChatChoice] -- ^ Completion candidates.
    , completionUsage :: Maybe Usage -- ^ Optional token accounting.
    , completionRaw :: Object -- ^ Full response, including provider extensions.
    } deriving (Eq, Show)

instance FromJSON ChatCompletion where
    parseJSON = withObject "ChatCompletion" $ \o -> ChatCompletion
        <$> o .: "id" <*> o .: "model" <*> o .: "choices" <*> o .:? "usage" <*> pure o

-- | Submit a non-streaming chat completion. Calls may incur provider charges.
-- This operation is never automatically retried or replayed on a redirect.
chatCompletion :: Client -> ChatRequest -> IO (Either HuggingFaceError ChatCompletion)
chatCompletion client request
    | T.null (T.strip (chatModel request)) = invalid "Chat model must be nonempty"
    | null (chatMessages request) = invalid "Chat messages must be nonempty"
    | maybe False (<= 0) (chatMaxTokens request) = invalid "max_tokens must be positive"
    | maybe False (not . inRange 0 2) (chatTemperature request) = invalid "temperature must be finite and between 0 and 2"
    | maybe False (\p -> not (inRange 0 1 p) || p == 0) (chatTopP request) = invalid "top_p must be finite and in (0,1]"
    | otherwise = postJSON client (requestFor client (clientInference client) ["v1", "chat", "completions"] []) request

-- | Parameters for the HF Inference text-generation task.
data GenerationParameters = GenerationParameters
    { generationMaxNewTokens :: Maybe Int -- ^ Positive output token limit.
    , generationTemperature :: Maybe Double -- ^ Positive finite sampling temperature.
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

newtype Generations = Generations [GeneratedText]

instance FromJSON Generations where
    parseJSON value = Generations <$> (parseJSON value <|> ((: []) <$> parseJSON value))

-- | Generate text using a model served by @hf-inference@. Task availability
-- depends on the model/provider; use 'chatCompletion' for router-hosted LLMs.
textGeneration :: Client -> RepoId -> Text -> GenerationParameters -> IO (Either HuggingFaceError [GeneratedText])
textGeneration client model prompt parameters
    | maybe False (<= 0) (generationMaxNewTokens parameters) = invalid "max_new_tokens must be positive"
    | maybe False (\t -> isNaN t || isInfinite t || t <= 0) (generationTemperature parameters) = invalid "Generation temperature must be positive and finite"
    | otherwise = fmap (\(Generations values) -> values) <$>
        infer client model (object ["inputs" .= prompt, "parameters" .= parameters, "stream" .= False])

-- | A predicted label and score.
data Classification = Classification
    { classificationLabel :: Text -- ^ Predicted label.
    , classificationScore :: Double -- ^ Model confidence score.
    } deriving (Eq, Show)

instance FromJSON Classification where
    parseJSON = withObject "Classification" $ \o -> Classification <$> o .: "label" <*> o .: "score"

-- | Classify one input. Accepts the flat and single-batch response shapes used
-- by HF Inference. Multiple batches for a single input are rejected.
classifyText :: Client -> RepoId -> Text -> IO (Either HuggingFaceError [Classification])
classifyText client model input = do
    result <- infer client model (object ["inputs" .= input])
    pure $ result >>= either (const (Left DecodeError)) Right . parseEither parseLabels
  where
    parseLabels value = parseJSON value <|> do
        batches <- parseJSON value :: Parser [[Classification]]
        case batches of
            [labels] -> pure labels
            _ -> fail "Expected one classification batch"

-- | Compute sentence embeddings for a nonempty batch. Uses the explicit
-- @/pipeline/feature-extraction@ route. Models returning token-level tensors
-- instead of one vector per input are outside this convenience function's schema.
featureExtraction :: Client -> RepoId -> [Text] -> IO (Either HuggingFaceError [[Double]])
featureExtraction client model inputs
    | null inputs = invalid "Embedding inputs must be nonempty"
    | otherwise = postJSON client
        (requestFor client (clientInference client)
            (["hf-inference", "models"] ++ T.splitOn "/" (repoIdText model) ++ ["pipeline", "feature-extraction"]) [])
        (object ["inputs" .= inputs])

-- | Send a caller-defined JSON request to a model's default HF Inference task.
-- Use this for task-specific request/response schemas beyond the typed helpers.
-- This does not route to arbitrary providers. Binary and SSE responses are not
-- decoded here. No model code executes locally, and requests are not retried.
infer :: (ToJSON request, FromJSON response) => Client -> RepoId -> request -> IO (Either HuggingFaceError response)
infer client model = postJSON client
    (requestFor client (clientInference client) (["hf-inference", "models"] ++ T.splitOn "/" (repoIdText model)) [])

invalid :: Text -> IO (Either HuggingFaceError a)
invalid = pure . Left . InvalidInput

inRange :: Double -> Double -> Double -> Bool
inRange low high value = not (isNaN value || isInfinite value) && value >= low && value <= high
