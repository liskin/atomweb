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

import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Text.Feed.Constructor (feedFromAtom)
import Text.Feed.Types (Feed, FeedKind(AtomKind))
import Text.Feed.Util (toFeedDateStringUTC)

import qualified Data.Text as T
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

getCurrentAtomDate :: IO Atom.Date
getCurrentAtomDate = mkAtomDate <$> getCurrentTime

mkAtomFeed :: Text -> Text -> Atom.Date -> [Atom.Entry] -> Feed
mkAtomFeed i title date entries =
    feedFromAtom feed0{ Atom.feedEntries = entries }
  where
    feed0 = Atom.nullFeed i (Atom.TextString title) date

mkAtomEntry :: Text -> Text -> Atom.Date -> Atom.EntryContent -> Atom.Entry
mkAtomEntry url title date content =
    entry0
        { Atom.entryLinks = [Atom.nullLink url]
        , Atom.entryContent = Just content
        }
  where
    entry0 = Atom.nullEntry url (Atom.TextString title) date

mkAtomText :: Text -> Atom.EntryContent
mkAtomText = Atom.TextContent

mkAtomHtml :: Text -> Atom.EntryContent
mkAtomHtml = Atom.HTMLContent

mkAtomDate :: UTCTime -> Atom.Date
mkAtomDate = T.pack . toFeedDateStringUTC AtomKind
