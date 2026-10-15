{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Types
-- Description : Validated identifiers, repository metadata, and client errors
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types
    ( Token, mkToken
    , RepoId, mkRepoId, repoIdText
    , Revision, mkRevision, revisionText, mainRevision
    , RepoPath, mkRepoPath, repoPathText
    , RepoType (..)
    , RepoInfo (..), RepoFile (..), UserInfo (..)
    , Page (..), NextPage
    , HuggingFaceError (..)
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char (isAscii, isAlphaNum, isControl)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import HuggingFace.Internal.Types

-- | Validate a bearer token. Its 'Show' instance never reveals the value.
mkToken :: Text -> Either HuggingFaceError Token
mkToken t
    | T.null t || not (T.all valid t) = Left (InvalidInput "Token must contain only ASCII letters, digits, underscores, or hyphens")
    | otherwise = Right (Token (encodeUtf8 t))
  where
    valid c = isAscii c && (isAlphaNum c || c == '_' || c == '-')

-- | A bare name or @owner/name@, following Hub repository-name restrictions.
mkRepoId :: Text -> Either HuggingFaceError RepoId
mkRepoId t
    | length parts `elem` [1, 2] && all valid parts = Right (RepoId t)
    | otherwise = Left (InvalidInput "Expected a repository name or owner/name with valid Hub name components")
  where
    parts = T.splitOn "/" t
    valid x = not (T.null x) && T.length x <= 96
        && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("-_." :: String))) x
        && T.head x `notElem` ("-." :: String)
        && T.last x `notElem` ("-." :: String)
        && not ("--" `T.isInfixOf` x || ".." `T.isInfixOf` x || ".git" `T.isSuffixOf` x)

-- | Branch, tag, or commit. Slashes are allowed and encoded as one URL segment.
mkRevision :: Text -> Either HuggingFaceError Revision
mkRevision t
    | all safeComponent (T.splitOn "/" t) = Right (Revision t)
    | otherwise = Left (InvalidInput "Revision must be nonempty and contain no control characters or dot-only segments")

-- | The default Hub branch.
mainRevision :: Revision
mainRevision = Revision "main"

-- | A relative repository path. Reject traversal, empty segments, and backslashes.
mkRepoPath :: Text -> Either HuggingFaceError RepoPath
mkRepoPath t
    | all safeComponent (T.splitOn "/" t) = Right (RepoPath t)
    | otherwise = Left (InvalidInput "Expected a relative repository file path without empty or dot segments")

safeComponent :: Text -> Bool
safeComponent t = not (T.null t) && t /= "." && t /= ".."
    && not (T.any (\c -> isControl c || c == '\\') t)

-- | Hub repository kinds.
data RepoType = Model | Dataset | Space
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Shared Hub metadata. Optional fields may be absent from list responses.
-- Unknown fields remain available in 'repoRaw' for forward compatibility.
data RepoInfo = RepoInfo
    { repoId :: Text -- ^ Fully qualified repository name.
    , repoSha :: Maybe Text -- ^ Revision commit SHA when supplied.
    , repoPrivate :: Maybe Bool -- ^ Visibility, when included by the server.
    , repoDownloads :: Maybe Integer -- ^ Download count, when available.
    , repoLikes :: Maybe Integer -- ^ Like count, when available.
    , repoTags :: [Text] -- ^ Hub metadata tags.
    , repoPipelineTag :: Maybe Text -- ^ Model task, when available.
    , repoLibraryName :: Maybe Text -- ^ Associated model library.
    , repoSdk :: Maybe Text -- ^ Space SDK, when available.
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
    { fileName :: Text -- ^ Relative path within the repository.
    , fileSize :: Maybe Integer -- ^ Byte count, when supplied.
    } deriving (Eq, Show)

instance FromJSON RepoFile where
    parseJSON = withObject "RepoFile" $ \o -> RepoFile <$> o .: "rfilename" <*> o .:? "size"

-- | Identity returned by @/api/whoami-v2@.
data UserInfo = UserInfo
    { userName :: Text -- ^ Account name.
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
