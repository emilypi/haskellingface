{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Repository
-- Description : Repository identity representations and composable parser
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Repository
    ( RepoId, parseRepoId, repoIdText, repoOwner, repoName, qualifiedRepo
    , RepoName, parseRepoName, repoNameText
    , AccountName, parseAccountName, accountNameText
    , Revision, parseRevision, revisionText, mainRevision, commitRevision
    , RepoPath, parseRepoPath, repoPathText
    , CommitSha, parseCommitSha, commitShaText
    , RepoType (..)
    , repoIdParser
    ) where

import Control.Monad (replicateM_)
import Data.Aeson
import Data.Char (isAscii, isHexDigit)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import HuggingFace.Error (HuggingFaceError)
import qualified HuggingFace.Internal.Parser as Grammar
import HuggingFace.Internal.Utils (TextParser, parseValue, parseTextJSON, matched)

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

repoIdParser :: TextParser RepoId
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
    CommitSha . T.toLower <$> matched (replicateM_ 40 (P.satisfy (\c -> isAscii c && isHexDigit c)))

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

-- | Hub repository kinds.
data RepoType = Model | Dataset | Space
    deriving (Eq, Ord, Show, Enum, Bounded)
