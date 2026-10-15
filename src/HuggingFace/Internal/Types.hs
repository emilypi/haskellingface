{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.Types
-- Description : Private client representation
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (Manager, Request)

-- | Opaque bearer token with a redacted 'Show' instance.
newtype Token = Token ByteString

instance Show Token where
    show _ = "<Token>"

-- | Validated Hub repository name.
newtype RepoId = RepoId
    { repoIdText :: Text -- ^ Recover the validated name.
    }
    deriving (Eq, Ord, Show)

-- | Branch, tag, or commit identifier.
newtype Revision = Revision
    { revisionText :: Text -- ^ Recover the revision text.
    }
    deriving (Eq, Ord, Show)

-- | Validated relative path inside a repository.
newtype RepoPath = RepoPath
    { repoPathText :: Text -- ^ Recover the repository path.
    }
    deriving (Eq, Ord, Show)

-- | Opaque server-provided pagination continuation.
newtype NextPage = NextPage String
    deriving (Eq)

instance Show NextPage where
    show _ = "<NextPage>"

-- | Reusable connection pool, endpoints, and credentials.
data Client = Client
    { clientManager :: Manager
    , clientHub :: Request
    , clientInference :: Request
    , clientToken :: Maybe Token
    , clientTimeout :: Int
    }
