{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : HuggingFace.Internal.HTTP.Utils
-- Description : HTTP path, origin, and bounded body utilities
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module HuggingFace.Internal.HTTP.Utils
    ( encodeSegments, readAtMost, sameOrigin, repoPlural
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (BodyReader, Request, brRead, secure, port, host)
import Network.HTTP.Types (urlEncode)
import HuggingFace.Types.Repository (RepoType (..))

encodeSegments :: [Text] -> ByteString
encodeSegments parts = "/" <> BS.intercalate "/" (map (urlEncode True . encodeUtf8) parts)

readAtMost :: Int -> BodyReader -> IO ByteString
readAtMost limit reader = BS.concat . reverse <$> go limit []
  where
    go n chunks
        | n <= 0 = pure chunks
        | otherwise = do
            chunk <- brRead reader
            if BS.null chunk then pure chunks
                else go (n - BS.length chunk) (BS.take n chunk : chunks)

sameOrigin :: Request -> Request -> Bool
sameOrigin a b = secure a == secure b && port a == port b
    && B8.map toLower (host a) == B8.map toLower (host b)

repoPlural :: RepoType -> Text
repoPlural Model = "models"
repoPlural Dataset = "datasets"
repoPlural Space = "spaces"
