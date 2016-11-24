{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main.Feed.KCBrno (kcBrnoDiskuzeFeed) where

import Control.Applicative ((<$>), pure)
import Data.Bool (Bool(True))
import Data.Function ((.), ($))
import Data.Maybe (Maybe(Just))
import Data.Monoid ((<>), mconcat)
import Data.String (String)
import System.IO (IO)

import Control.Lens ((^.))
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import qualified Data.Text.Lazy as Text (Text, unpack, stripPrefix)
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8)
import qualified Network.Wreq as Wreq (get, responseBody)
import qualified Text.HTML.TagSoup as TS (innerText)
import qualified Text.HTML.TagSoup.Tree as TS
    ( TagTree(TagBranch)
    , flattenTree
    , parseTree
    , renderTree
    , transformTree
    , universeTree
    )

import Main.Feed
    ( Feed
    , getCurrentAtomDate
    , mkAtomDate
    , mkAtomEntry
    , mkAtomFeed
    , mkAtomHtml
    )

kcBrnoDiskuzeFeed :: IO Feed
kcBrnoDiskuzeFeed = do
    now <- getCurrentAtomDate
    tree <- getTags "http://www.kcbrno.org/diskuze.php"
    pure $ mkAtomFeed "https://github.com/liskin/atomweb#kcBrnoDiskuzeFeed" "kcbrno.org diskuze" now $ do
        TS.TagBranch "div" [("class", "prispevek")] prispevek <- TS.universeTree tree
        let date = getDate prispevek
        let atomDate = toAtomDate date
        let entryUri = "http://www.kcbrno.org/diskuze.php#date_" <> atomDate
        let entryTitle = date
        let entryContent = mkAtomHtml $ Text.unpack $ TS.renderTree $ fixLinks prispevek
        pure $ mkAtomEntry entryUri entryTitle atomDate entryContent
  where
    getDate p = Text.unpack date
      where
        date = TS.innerText $ mconcat
            [ TS.flattenTree ts | TS.TagBranch "div" [("class", "datum")] ts <- TS.universeTree p ]

    toAtomDate = mkAtomDate . parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

    fixLinks = TS.transformTree f
      where
        f (TS.TagBranch "img" attrs inner) = [TS.TagBranch "img" (fixSrc <$> attrs) inner]
        f x = [x]

        fixSrc ("src", Text.stripPrefix "./" -> Just url) = ("src", "http://www.kcbrno.org/" <> url)
        fixSrc x = x

getTags :: String -> IO [TS.TagTree Text.Text]
getTags = (TS.parseTree . Text.decodeUtf8 . (^. Wreq.responseBody) <$>) . Wreq.get
