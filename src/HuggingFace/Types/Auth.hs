-- |
-- Module      : HuggingFace.Types.Auth
-- Description : Opaque authentication tokens
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Types.Auth
    ( Token, parseToken
    ) where

import HuggingFace.Internal.Auth (Token, parseToken)
