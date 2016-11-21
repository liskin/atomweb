{-# LANGUAGE NoImplicitPrelude #-}

module Main.Feed.Example (exampleFeed) where

import Control.Applicative (pure)
import Data.Function (($))
import System.IO (IO)

import Main.Feed
    ( Feed
    , getCurrentAtomDate
    , mkAtomEntry
    , mkAtomFeed
    , mkAtomText
    )

exampleFeed :: IO Feed
exampleFeed = do
    now <- getCurrentAtomDate
    pure $ mkAtomFeed "nomi.cz/example" "example" now
        [ mkAtomEntry "http://example.com/" "example.com" now (mkAtomText "xxx")
        ]
