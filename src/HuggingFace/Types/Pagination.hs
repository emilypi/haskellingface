-- |
-- Module      : HuggingFace.Types.Pagination
-- Description : Explicit pages with opaque continuation handles
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Pagination
    ( Page (..), NextPage
    ) where

import HuggingFace.Internal.Pagination (NextPage)

-- | One server page. The caller explicitly decides whether to fetch another.
data Page a = Page
    { pageItems :: [a] -- ^ Items in this response.
    , pageNext :: Maybe NextPage -- ^ Opaque continuation supplied by the Hub.
    } deriving (Eq, Show)
