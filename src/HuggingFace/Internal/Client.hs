-- |
-- Module      : HuggingFace.Internal.Client
-- Description : Private client connection and credential state
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.Client
    ( Client (..)
    ) where

import Network.HTTP.Client (Manager, Request)
import HuggingFace.Types.Auth (Token)

-- | Reusable connection pool, endpoints, and credentials.
data Client = Client
    { clientManager :: Manager
    , clientHub :: Request
    , clientInference :: Request
    , clientToken :: Maybe Token
    , clientTimeout :: Int
    }
