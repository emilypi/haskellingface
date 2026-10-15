{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Small command-line examples of the Hugging Face client
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import HuggingFace
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

-- | Run a Hub query or an explicitly requested hosted chat completion.
main :: IO ()
main = do
    arguments <- map T.pack <$> getArgs
    tokenText <- lookupEnv "HF_TOKEN"
    token <- orDie (traverse (parseToken . T.pack) tokenText)
    client <- newClient defaultClientConfig {accessToken = token}
    case arguments of
        ["models", query] -> do
            limit <- orDie (mkPositiveInt 10)
            page <- listModels client defaultListOptions {listSearch = Just query, listLimit = Just limit} >>= orDie
            mapM_ (T.putStrLn . repoIdText . repoId) (pageItems page)
        ["model", name] -> do
            repository <- orDie (parseRepoId name)
            info <- modelInfo client repository Nothing >>= orDie
            print info
        ["chat", model, prompt] -> do
            target <- orDie (parseInferenceModel model)
            completion <- chatCompletion client (defaultChatRequest target (userMessage prompt :| [])) >>= orDie
            mapM_ (print . choiceMessage) (completionChoices completion)
        _ -> die "Usage: huggingface-example (models SEARCH | model OWNER/NAME | chat MODEL PROMPT)"

orDie :: Show e => Either e a -> IO a
orDie = either (die . show) pure
