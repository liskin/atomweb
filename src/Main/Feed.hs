{-# LANGUAGE NoImplicitPrelude #-}

module Main.Feed
    ( Atom.Date
    , Atom.Entry
    , Feed
    , getCurrentAtomDate
    , mkAtomDate
    , mkAtomEntry
    , mkAtomFeed
    , mkAtomHtml
    , mkAtomText
    )
  where

import Control.Applicative ((<$>))
import Data.Maybe (Maybe(Just))
import Data.String (String)
import System.IO (IO)

import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Text.Atom.Feed as Atom
    ( Date
    , Entry(entryContent, entryLinks)
    , EntryContent(HTMLContent, TextContent)
    , Feed(feedEntries)
    , TextContent(TextString)
    , nullEntry
    , nullFeed
    , nullLink
    )
import Text.Feed.Constructor (feedFromAtom)
import Text.Feed.Types (Feed, FeedKind(AtomKind))
import Text.Feed.Util (toFeedDateStringUTC)

getCurrentAtomDate :: IO Atom.Date
getCurrentAtomDate = mkAtomDate <$> getCurrentTime

mkAtomFeed :: String -> String -> Atom.Date -> [Atom.Entry] -> Feed
mkAtomFeed id title date entries =
    feedFromAtom feed0{ Atom.feedEntries = entries }
  where
    feed0 = Atom.nullFeed id (Atom.TextString title) date

mkAtomEntry :: String -> String -> Atom.Date -> Atom.EntryContent -> Atom.Entry
mkAtomEntry url title date content =
    entry0
        { Atom.entryLinks = [Atom.nullLink url]
        , Atom.entryContent = Just content
        }
  where
    entry0 = Atom.nullEntry url (Atom.TextString title) date

mkAtomText :: String -> Atom.EntryContent
mkAtomText = Atom.TextContent

mkAtomHtml :: String -> Atom.EntryContent
mkAtomHtml = Atom.HTMLContent

mkAtomDate :: UTCTime -> Atom.Date
mkAtomDate = toFeedDateStringUTC AtomKind
