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
import Network.HTTP.Client (Manager, Request)

-- | Opaque bearer token with a redacted 'Show' instance.
newtype Token = Token ByteString

instance Show Token where
    show _ = "<Token>"

-- | Opaque server-provided pagination continuation.
newtype NextPage = NextPage ByteString
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
