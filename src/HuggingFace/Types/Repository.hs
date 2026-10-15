-- |
-- Module      : HuggingFace.Types.Repository
-- Description : Repository names, revisions, paths, and commit identities
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Repository
    ( RepoId, parseRepoId, repoIdText, repoOwner, repoName, qualifiedRepo
    , RepoName, parseRepoName, repoNameText
    , AccountName, parseAccountName, accountNameText
    , Revision, parseRevision, revisionText, mainRevision, commitRevision
    , RepoPath, parseRepoPath, repoPathText
    , CommitSha, parseCommitSha, commitShaText
    , RepoType (..)
    ) where

import HuggingFace.Internal.Repository
