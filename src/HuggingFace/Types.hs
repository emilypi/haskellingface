{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types
-- Description : Parsed domain values, repository metadata, and client errors
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types
    ( Token, parseToken
    , RepoId, parseRepoId, repoIdText, repoOwner, repoName, qualifiedRepo
    , RepoName, parseRepoName, repoNameText
    , Revision, parseRevision, revisionText, mainRevision, commitRevision
    , RepoPath, parseRepoPath, repoPathText
    , CommitSha, parseCommitSha, commitShaText
    , AccountName, parseAccountName, accountNameText
    , HubTag, parseHubTag, hubTagText
    , PipelineTag, parsePipelineTag, pipelineTagText
    , LibraryName, parseLibraryName, libraryNameText
    , SpaceSdk, parseSpaceSdk, spaceSdkText
    , SortKey, parseSortKey, sortKeyText
    , ChatRole, parseChatRole, chatRoleText
    , ToolCallId, parseToolCallId, toolCallIdText
    , ParticipantName, parseParticipantName, participantNameText
    , CompletionId, parseCompletionId, completionIdText
    , ModelName, parseModelName, modelNameText
    , FinishReason, parseFinishReason, finishReasonText
    , Provider, parseProvider, providerText
    , systemRole, userRole, assistantRole, toolRole, developerRole
    , Routing (..), InferenceModel, parseInferenceModel, inferenceModelText
    , modelRepository, modelRouting, modelFromRepo, routeModel
    , PositiveInt, mkPositiveInt, positiveIntValue, defaultTimeout
    , ChatTemperature, mkChatTemperature, chatTemperatureValue
    , TopP, mkTopP, topPValue
    , GenerationTemperature, mkGenerationTemperature, generationTemperatureValue
    , RepoType (..)
    , RepoInfo (..), RepoFile (..), UserInfo (..)
    , Page (..), NextPage
    , HuggingFaceError (..)
    ) where

import Control.Monad (replicateM_)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Char (isAscii, isHexDigit)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Parsec as P
import qualified HuggingFace.Internal.Parser as Grammar
import HuggingFace.Internal.Types (Token (..), NextPage)

-- | Parse a bearer token without retaining malformed input in errors.
-- Its 'Show' instance never reveals the value.
parseToken :: Text -> Either HuggingFaceError Token
parseToken = parseValue "Token requires ASCII letters, digits, underscores, or hyphens" $
    Token . encodeUtf8 <$> Grammar.takeWhile1P (\c -> Grammar.asciiAlphaNum c || c == '_' || c == '-')

-- | Repository basename, distinct from its optional owner.
newtype RepoName = RepoName Text deriving (Eq, Ord, Show)

-- | Parse a Hub repository basename.
parseRepoName :: Text -> Either HuggingFaceError RepoName
parseRepoName = parseValue "Expected a Hub repository basename" (RepoName <$> Grammar.name)

-- | Render a repository basename.
repoNameText :: RepoName -> Text
repoNameText (RepoName t) = t

instance ToJSON RepoName where toJSON = toJSON . repoNameText
instance FromJSON RepoName where parseJSON = parseTextJSON "RepoName" parseRepoName

-- | Parsed repository identity. The constructor is hidden: components can only
-- be assembled from parsed names, and the owner can never contain a slash.
newtype RepoId = RepoId (Maybe AccountName, RepoName) deriving (Eq, Ord, Show)

-- | Parse either a bare repository name or @owner/name@.
parseRepoId :: Text -> Either HuggingFaceError RepoId
parseRepoId = parseValue "Expected a repository name or owner/name" repoIdParser

repoIdParser :: Grammar.Parser RepoId
repoIdParser = do
    first <- Grammar.name
    P.option (RepoId (Nothing, RepoName first)) $
        qualifiedRepo (AccountName first) . RepoName <$> (P.char '/' *> Grammar.name)

-- | Assemble a qualified repository from parsed components without reparsing.
qualifiedRepo :: AccountName -> RepoName -> RepoId
qualifiedRepo owner name = RepoId (Just owner, name)

-- | Render a repository identity for display or persistence.
repoIdText :: RepoId -> Text
repoIdText (RepoId (owner, name)) = maybe "" ((<> "/") . accountNameText) owner <> repoNameText name

-- | Optional owner of a repository.
repoOwner :: RepoId -> Maybe AccountName
repoOwner (RepoId (owner, _)) = owner

-- | Repository basename.
repoName :: RepoId -> RepoName
repoName (RepoId (_, name)) = name

instance ToJSON RepoId where toJSON = toJSON . repoIdText
instance FromJSON RepoId where parseJSON = parseTextJSON "RepoId" parseRepoId

-- | Branch, tag, or commit reference; slash-separated components are retained.
newtype Revision = Revision (NonEmpty Text) deriving (Eq, Ord, Show)

-- | Parse a revision. Slashes are encoded together as one URL segment.
parseRevision :: Text -> Either HuggingFaceError Revision
parseRevision = parseValue "Expected nonempty revision segments without traversal, controls, or backslashes" (Revision <$> Grammar.components)

-- | Render a revision.
revisionText :: Revision -> Text
revisionText (Revision parts) = T.intercalate "/" (NE.toList parts)

-- | The default Hub branch.
mainRevision :: Revision
mainRevision = Revision ("main" :| [])

-- | Pin a revision to an already parsed full commit SHA.
commitRevision :: CommitSha -> Revision
commitRevision sha = Revision (commitShaText sha :| [])

instance ToJSON Revision where toJSON = toJSON . revisionText
instance FromJSON Revision where parseJSON = parseTextJSON "Revision" parseRevision

-- | Nonempty relative file path with no empty, dot, or parent segments.
newtype RepoPath = RepoPath (NonEmpty Text) deriving (Eq, Ord, Show)

-- | Parse a repository-relative path, preserving Unicode and spaces.
parseRepoPath :: Text -> Either HuggingFaceError RepoPath
parseRepoPath = parseValue "Expected a relative repository file path without traversal, controls, or backslashes" (RepoPath <$> Grammar.components)

-- | Render a repository-relative path.
repoPathText :: RepoPath -> Text
repoPathText (RepoPath parts) = T.intercalate "/" (NE.toList parts)

instance ToJSON RepoPath where toJSON = toJSON . repoPathText
instance FromJSON RepoPath where parseJSON = parseTextJSON "RepoPath" parseRepoPath

-- | Full SHA-1 commit identifier, stored in canonical lowercase form.
newtype CommitSha = CommitSha Text deriving (Eq, Ord, Show)

-- | Parse exactly forty ASCII hexadecimal digits. Use 'parseRevision' for
-- abbreviated commit references, branches, or tags.
parseCommitSha :: Text -> Either HuggingFaceError CommitSha
parseCommitSha = parseValue "CommitSha requires forty hexadecimal digits" $
    CommitSha . T.toLower <$> Grammar.matched (replicateM_ 40 (P.satisfy (\c -> isAscii c && isHexDigit c)))

-- | Render a full commit identifier.
commitShaText :: CommitSha -> Text
commitShaText (CommitSha t) = t

instance ToJSON CommitSha where toJSON = toJSON . commitShaText
instance FromJSON CommitSha where parseJSON = parseTextJSON "CommitSha" parseCommitSha

-- | Account or organization namespace.
newtype AccountName = AccountName Text deriving (Eq, Ord, Show)

-- | Parse a AccountName from external text.
parseAccountName :: Text -> Either HuggingFaceError AccountName
parseAccountName = parseValue "Expected a Hub account or organization name" (AccountName <$> Grammar.name)

-- | Render a AccountName.
accountNameText :: AccountName -> Text
accountNameText (AccountName t) = t

instance ToJSON AccountName where toJSON = toJSON . accountNameText
instance FromJSON AccountName where parseJSON = parseTextJSON "AccountName" parseAccountName

-- | Open Hub metadata tag, including namespaced tags.
newtype HubTag = HubTag Text deriving (Eq, Ord, Show)

-- | Parse a HubTag from external text.
parseHubTag :: Text -> Either HuggingFaceError HubTag
parseHubTag = fmap HubTag . parseLabel "HubTag"

-- | Render a HubTag.
hubTagText :: HubTag -> Text
hubTagText (HubTag t) = t

instance ToJSON HubTag where toJSON = toJSON . hubTagText
instance FromJSON HubTag where parseJSON = parseTextJSON "HubTag" parseHubTag

-- | Open model task identifier.
newtype PipelineTag = PipelineTag Text deriving (Eq, Ord, Show)

-- | Parse a PipelineTag from external text.
parsePipelineTag :: Text -> Either HuggingFaceError PipelineTag
parsePipelineTag = fmap PipelineTag . parseLabel "PipelineTag"

-- | Render a PipelineTag.
pipelineTagText :: PipelineTag -> Text
pipelineTagText (PipelineTag t) = t

instance ToJSON PipelineTag where toJSON = toJSON . pipelineTagText
instance FromJSON PipelineTag where parseJSON = parseTextJSON "PipelineTag" parsePipelineTag

-- | Open model library identifier.
newtype LibraryName = LibraryName Text deriving (Eq, Ord, Show)

-- | Parse a LibraryName from external text.
parseLibraryName :: Text -> Either HuggingFaceError LibraryName
parseLibraryName = fmap LibraryName . parseLabel "LibraryName"

-- | Render a LibraryName.
libraryNameText :: LibraryName -> Text
libraryNameText (LibraryName t) = t

instance ToJSON LibraryName where toJSON = toJSON . libraryNameText
instance FromJSON LibraryName where parseJSON = parseTextJSON "LibraryName" parseLibraryName

-- | Open Space SDK identifier.
newtype SpaceSdk = SpaceSdk Text deriving (Eq, Ord, Show)

-- | Parse a SpaceSdk from external text.
parseSpaceSdk :: Text -> Either HuggingFaceError SpaceSdk
parseSpaceSdk = fmap SpaceSdk . parseLabel "SpaceSdk"

-- | Render a SpaceSdk.
spaceSdkText :: SpaceSdk -> Text
spaceSdkText (SpaceSdk t) = t

instance ToJSON SpaceSdk where toJSON = toJSON . spaceSdkText
instance FromJSON SpaceSdk where parseJSON = parseTextJSON "SpaceSdk" parseSpaceSdk

-- | Open Hub sort field.
newtype SortKey = SortKey Text deriving (Eq, Ord, Show)

-- | Parse a SortKey from external text.
parseSortKey :: Text -> Either HuggingFaceError SortKey
parseSortKey = fmap SortKey . parseLabel "SortKey"

-- | Render a SortKey.
sortKeyText :: SortKey -> Text
sortKeyText (SortKey t) = t

instance ToJSON SortKey where toJSON = toJSON . sortKeyText
instance FromJSON SortKey where parseJSON = parseTextJSON "SortKey" parseSortKey

-- | Open chat role; common roles have named constants.
newtype ChatRole = ChatRole Text deriving (Eq, Ord, Show)

-- | Parse a ChatRole from external text.
parseChatRole :: Text -> Either HuggingFaceError ChatRole
parseChatRole = fmap ChatRole . parseLabel "ChatRole"

-- | Render a ChatRole.
chatRoleText :: ChatRole -> Text
chatRoleText (ChatRole t) = t

instance ToJSON ChatRole where toJSON = toJSON . chatRoleText
instance FromJSON ChatRole where parseJSON = parseTextJSON "ChatRole" parseChatRole

-- | Opaque provider-assigned tool call identifier.
newtype ToolCallId = ToolCallId Text deriving (Eq, Ord, Show)

-- | Parse a ToolCallId from external text.
parseToolCallId :: Text -> Either HuggingFaceError ToolCallId
parseToolCallId = fmap ToolCallId . parseLabel "ToolCallId"

-- | Render a ToolCallId.
toolCallIdText :: ToolCallId -> Text
toolCallIdText (ToolCallId t) = t

instance ToJSON ToolCallId where toJSON = toJSON . toolCallIdText
instance FromJSON ToolCallId where parseJSON = parseTextJSON "ToolCallId" parseToolCallId

-- | Open chat participant name; provider-specific naming rules remain server-side.
newtype ParticipantName = ParticipantName Text deriving (Eq, Ord, Show)

-- | Parse a ParticipantName from external text.
parseParticipantName :: Text -> Either HuggingFaceError ParticipantName
parseParticipantName = fmap ParticipantName . parseLabel "ParticipantName"

-- | Render a ParticipantName.
participantNameText :: ParticipantName -> Text
participantNameText (ParticipantName t) = t

instance ToJSON ParticipantName where toJSON = toJSON . participantNameText
instance FromJSON ParticipantName where parseJSON = parseTextJSON "ParticipantName" parseParticipantName

-- | Opaque provider-assigned completion identifier. An empty identifier is
-- preserved because Hugging Face / TGI can return it on successful responses.
newtype CompletionId = CompletionId Text deriving (Eq, Ord, Show)

-- | Parse a CompletionId from external text.
parseCompletionId :: Text -> Either HuggingFaceError CompletionId
parseCompletionId = parseValue "Expected a completion identifier without controls" $
    CompletionId <$> (Grammar.label P.<|> ("" <$ P.eof))

-- | Render a CompletionId.
completionIdText :: CompletionId -> Text
completionIdText (CompletionId t) = t

instance ToJSON CompletionId where toJSON = toJSON . completionIdText
instance FromJSON CompletionId where parseJSON = parseTextJSON "CompletionId" parseCompletionId

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

-- | Open provider stop reason; unknown reasons are preserved.
newtype FinishReason = FinishReason Text deriving (Eq, Ord, Show)

-- | Parse a FinishReason from external text.
parseFinishReason :: Text -> Either HuggingFaceError FinishReason
parseFinishReason = fmap FinishReason . parseLabel "FinishReason"

-- | Render a FinishReason.
finishReasonText :: FinishReason -> Text
finishReasonText (FinishReason t) = t

instance ToJSON FinishReason where toJSON = toJSON . finishReasonText
instance FromJSON FinishReason where parseJSON = parseTextJSON "FinishReason" parseFinishReason

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

-- | System instruction role.
systemRole :: ChatRole
systemRole = ChatRole "system"

-- | User message role.
userRole :: ChatRole
userRole = ChatRole "user"

-- | Assistant response role.
assistantRole :: ChatRole
assistantRole = ChatRole "assistant"

-- | Tool response role.
toolRole :: ChatRole
toolRole = ChatRole "tool"

-- | Developer instruction role.
developerRole :: ChatRole
developerRole = ChatRole "developer"

-- | Router selection policy or a named provider. An absent routing choice uses
-- the server default; it is distinct from explicitly selecting 'Fastest'.
data Routing = Fastest | Cheapest | Preferred | Via Provider deriving (Eq, Ord, Show)

-- | A Hub model repository paired with an optional parsed routing choice.
newtype InferenceModel = InferenceModel (RepoId, Maybe Routing) deriving (Eq, Ord, Show)

-- | Parse @owner/model@ or @owner/model:provider-or-policy@.
parseInferenceModel :: Text -> Either HuggingFaceError InferenceModel
parseInferenceModel = parseValue "Expected a model repository with at most one routing suffix" $
    InferenceModel <$> ((,) <$> repoIdParser <*> P.optionMaybe (P.char ':' *> routingParser))

routingParser :: Grammar.Parser Routing
routingParser = do
    slug <- Grammar.providerSlug
    pure $ case slug of
        "fastest" -> Fastest
        "cheapest" -> Cheapest
        "preferred" -> Preferred
        _ -> Via (Provider slug)

providerParser :: Grammar.Parser Provider
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

parseTextJSON :: Text -> (Text -> Either HuggingFaceError a) -> Value -> Parser a
parseTextJSON name parse = withText (T.unpack name) (fromEither . parse)

fromEither :: Either HuggingFaceError a -> Parser a
fromEither = either (fail . show) pure

parseValue :: Text -> Grammar.Parser a -> Text -> Either HuggingFaceError a
parseValue message = Grammar.parseText (InvalidInput message)

parseLabel :: Text -> Text -> Either HuggingFaceError Text
parseLabel name = parseValue (name <> " requires nonblank text without control characters") Grammar.label

inRange :: Double -> Double -> Double -> Bool
inRange low high t = not (isNaN t || isInfinite t) && t >= low && t <= high

-- | Hub repository kinds.
data RepoType = Model | Dataset | Space
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Shared Hub metadata. Optional fields may be absent from list responses.
-- Unknown fields remain available in 'repoRaw' for forward compatibility.
data RepoInfo = RepoInfo
    { repoId :: RepoId -- ^ Fully qualified repository name.
    , repoSha :: Maybe CommitSha -- ^ Revision commit SHA when supplied.
    , repoPrivate :: Maybe Bool -- ^ Visibility, when included by the server.
    , repoDownloads :: Maybe Integer -- ^ Download count, when available.
    , repoLikes :: Maybe Integer -- ^ Like count, when available.
    , repoTags :: [HubTag] -- ^ Hub metadata tags.
    , repoPipelineTag :: Maybe PipelineTag -- ^ Model task, when available.
    , repoLibraryName :: Maybe LibraryName -- ^ Associated model library.
    , repoSdk :: Maybe SpaceSdk -- ^ Space SDK, when available.
    , repoSiblings :: [RepoFile] -- ^ Files included in the metadata response.
    , repoRaw :: Object -- ^ Complete JSON object, including provider extensions.
    } deriving (Eq, Show)

instance FromJSON RepoInfo where
    parseJSON = withObject "RepoInfo" $ \o -> RepoInfo
        <$> o .: "id" <*> o .:? "sha" <*> o .:? "private"
        <*> o .:? "downloads" <*> o .:? "likes" <*> o .:? "tags" .!= []
        <*> o .:? "pipeline_tag" <*> o .:? "library_name" <*> o .:? "sdk"
        <*> o .:? "siblings" .!= [] <*> pure o

-- | A file returned in repository metadata. Size requires @blobs=true@.
data RepoFile = RepoFile
    { fileName :: RepoPath -- ^ Relative path within the repository.
    , fileSize :: Maybe Integer -- ^ Byte count, when supplied.
    } deriving (Eq, Show)

instance FromJSON RepoFile where
    parseJSON = withObject "RepoFile" $ \o -> RepoFile <$> o .: "rfilename" <*> o .:? "size"

-- | Identity returned by @/api/whoami-v2@.
data UserInfo = UserInfo
    { userName :: AccountName -- ^ Account name.
    , userFullName :: Maybe Text -- ^ Display name.
    , userRaw :: Object -- ^ Full identity response; may contain private account data.
    } deriving (Eq, Show)

instance FromJSON UserInfo where
    parseJSON = withObject "UserInfo" $ \o -> UserInfo <$> o .: "name" <*> o .:? "fullname" <*> pure o

-- | One server page. The caller explicitly decides whether to fetch another.
data Page a = Page
    { pageItems :: [a] -- ^ Items in this response.
    , pageNext :: Maybe NextPage -- ^ Opaque continuation supplied by the Hub.
    } deriving (Eq, Show)

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
