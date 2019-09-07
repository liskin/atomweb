{-# LANGUAGE OverloadedStrings #-}

module Main.Feed.Reddit
    ( subRedditFeed
    )
  where

import Data.Text (Text)

import Main.Feed
    ( Feed
    , getCurrentAtomDate
    , mkAtomFeed
    )

subRedditFeed :: Text -> Maybe Int -> IO Feed
subRedditFeed r _minScore = do
    now <- getCurrentAtomDate
    let feedId = "https://github.com/liskin/atomweb#subRedditFeed_" <> r
    pure $ mkAtomFeed feedId r now []
