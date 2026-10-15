{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types.Routing
-- Description : Inference model identities and provider selection
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Routing
    ( Provider, parseProvider, providerText
    , Routing (..), InferenceModel, parseInferenceModel, inferenceModelText
    , modelRepository, modelRouting, modelFromRepo, routeModel
    , ModelName, parseModelName, modelNameText
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError)
import qualified HuggingFace.Internal.Parser as Grammar
import HuggingFace.Internal.Repository (repoIdParser)
import HuggingFace.Internal.Utils (TextParser, parseValue, parseTextJSON, parseLabel)
import HuggingFace.Types.Repository (RepoId, repoIdText)

-- | Provider-resolved model name; it need not be a Hub repository ID.
newtype ModelName = ModelName Text deriving (Eq, Ord, Show)

-- | Parse a ModelName from external text.
parseModelName :: Text -> Either HuggingFaceError ModelName
parseModelName = fmap ModelName . parseLabel "ModelName"

-- | Render a ModelName.
modelNameText :: ModelName -> Text
modelNameText (ModelName t) = t

instance ToJSON ModelName where toJSON = toJSON . modelNameText
instance FromJSON ModelName where parseJSON = parseTextJSON "ModelName" parseModelName

-- | Provider routing slug; policy names are reserved for Routing constructors.
newtype Provider = Provider Text deriving (Eq, Ord, Show)

-- | Parse a Provider from external text.
parseProvider :: Text -> Either HuggingFaceError Provider
parseProvider = parseValue "Expected a provider slug, not a routing policy" providerParser

-- | Render a Provider.
providerText :: Provider -> Text
providerText (Provider t) = t

instance ToJSON Provider where toJSON = toJSON . providerText
instance FromJSON Provider where parseJSON = parseTextJSON "Provider" parseProvider

-- | Router selection policy or a named provider. An absent routing choice uses
-- the server default; it is distinct from explicitly selecting 'Fastest'.
data Routing = Fastest | Cheapest | Preferred | Via Provider deriving (Eq, Ord, Show)

-- | A Hub model repository paired with an optional parsed routing choice.
newtype InferenceModel = InferenceModel (RepoId, Maybe Routing) deriving (Eq, Ord, Show)

-- | Parse @owner/model@ or @owner/model:provider-or-policy@.
parseInferenceModel :: Text -> Either HuggingFaceError InferenceModel
parseInferenceModel = parseValue "Expected a model repository with at most one routing suffix" $
    InferenceModel <$> ((,) <$> repoIdParser <*> P.optionMaybe (P.char ':' *> routingParser))

routingParser :: TextParser Routing
routingParser = do
    slug <- Grammar.providerSlug
    pure $ case slug of
        "fastest" -> Fastest
        "cheapest" -> Cheapest
        "preferred" -> Preferred
        _ -> Via (Provider slug)

providerParser :: TextParser Provider
providerParser = do
    routing <- routingParser
    case routing of
        Via provider -> pure provider
        _ -> P.parserFail "Reserved routing policy"

-- | Choose the router's default policy for an existing repository identifier.
modelFromRepo :: RepoId -> InferenceModel
modelFromRepo repository = InferenceModel (repository, Nothing)

-- | Select a routing policy without converting an identifier back to text.
routeModel :: RepoId -> Routing -> InferenceModel
routeModel repository routing = InferenceModel (repository, Just routing)

-- | The repository referenced by a routed model.
modelRepository :: InferenceModel -> RepoId
modelRepository (InferenceModel (repository, _)) = repository

-- | Optional explicit routing choice.
modelRouting :: InferenceModel -> Maybe Routing
modelRouting (InferenceModel (_, routing)) = routing

-- | Render a model together with its optional routing suffix.
inferenceModelText :: InferenceModel -> Text
inferenceModelText (InferenceModel (repository, routing)) = repoIdText repository <> maybe "" ((":" <>) . render) routing
  where
    render Fastest = "fastest"
    render Cheapest = "cheapest"
    render Preferred = "preferred"
    render (Via provider) = providerText provider

instance ToJSON InferenceModel where toJSON = toJSON . inferenceModelText
instance FromJSON InferenceModel where parseJSON = parseTextJSON "InferenceModel" parseInferenceModel
