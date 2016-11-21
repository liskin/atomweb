{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main.Feed.KCBrno (kcBrnoDiskuzeFeed) where

import Control.Applicative ((<$>), pure)
import Data.Bool (Bool(True))
import Data.Function ((.), ($))
import Data.List (head)
import Data.Maybe (Maybe(Just))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO)

import Control.Lens ((^.))
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import qualified Data.Text.Lazy as Text (Text, unpack, stripPrefix)
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8)
import qualified Network.Wreq as Wreq (get, responseBody)
import qualified Text.HTML.TagSoup as TS (Tag(TagText))
import qualified Text.HTML.TagSoup.Tree as TS
    ( TagTree(TagBranch, TagLeaf)
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
        pure
            $ mkAtomEntry ("http://www.kcbrno.org/diskuze.php#date_" <> atomDate) date atomDate
            $ mkAtomHtml $ Text.unpack $ TS.renderTree $ fixLinks prispevek
  where
    getDate p = Text.unpack date
      where
        date = head [ d | TS.TagBranch "div" [("class", "datum")] [TS.TagLeaf (TS.TagText d)] <- TS.universeTree p ]

    toAtomDate = mkAtomDate . parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

    fixLinks = TS.transformTree f
      where
        f (TS.TagBranch "img" attrs inner) = [TS.TagBranch "img" (fixSrc <$> attrs) inner]
        f x = [x]

        fixSrc ("src", Text.stripPrefix "./" -> Just url) = ("src", "http://www.kcbrno.org/" <> url)
        fixSrc x = x

getTags :: String -> IO [TS.TagTree Text.Text]
getTags = (TS.parseTree . Text.decodeUtf8 . (^. Wreq.responseBody) <$>) . Wreq.get
