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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import HuggingFace
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

-- | Run a Hub query or an explicitly requested hosted chat completion.
main :: IO ()
main = do
    arguments <- getArgs
    tokenText <- lookupEnv "HF_TOKEN"
    token <- orDie (traverse (mkToken . T.pack) tokenText)
    client <- newClient defaultClientConfig {accessToken = token} >>= orDie
    case arguments of
        ["models", query] -> do
            page <- listModels client defaultListOptions {listSearch = Just (T.pack query), listLimit = Just 10} >>= orDie
            mapM_ (T.putStrLn . repoId) (pageItems page)
        ["model", name] -> do
            repository <- orDie (mkRepoId (T.pack name))
            info <- modelInfo client repository Nothing >>= orDie
            print info
        ["chat", model, prompt] -> do
            completion <- chatCompletion client (defaultChatRequest (T.pack model) [userMessage (T.pack prompt)]) >>= orDie
            mapM_ (print . choiceMessage) (completionChoices completion)
        _ -> die "Usage: huggingface-example (models SEARCH | model OWNER/NAME | chat MODEL PROMPT)"

orDie :: Show e => Either e a -> IO a
orDie = either (die . show) pure
