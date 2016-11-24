{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main.Feed.KCBrno
    ( kcBrnoDiskuzeFeed
    , kcBrnoZpravyFeed
    )
  where

import Control.Applicative ((<$>), pure)
import Control.Monad ((>>=), forM)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool(True))
import Data.Eq ((==))
import Data.Function ((.), ($))
import Data.List (tails)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mconcat)
import Data.String (String)
import System.IO (IO)

import Control.Lens ((^.))
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Database.Persist ((=.), entityKey, entityVal, getBy, insert_, update)
import Database.Persist.Sqlite (SqlPersistM, runSqlite, runMigration)
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistUpperCase
    , share
    , sqlSettings
    )
import qualified Data.Text.Lazy as Text (Text, intercalate, stripPrefix, unpack)
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8, encodeUtf8)
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
    ( Date
    , Feed
    , getCurrentAtomDate
    , mkAtomDate
    , mkAtomEntry
    , mkAtomFeed
    , mkAtomHtml
    )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Zprava
        name Text.Text
        sha1 String
        lastUpdate String
        ZpravaUniqueName name
|]

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

kcBrnoZpravyFeed :: IO Feed
kcBrnoZpravyFeed = do
    now <- getCurrentAtomDate
    tree <- getTags "http://www.kcbrno.org/zpravy.php"
    let zpravy =
            [ (name, heading, description)
            | TS.TagBranch "td" [("id", "center")] content <- TS.universeTree tree
            , ( TS.TagBranch "a" [("name", name)] []
              : TS.TagBranch "h4" [] heading
              : TS.TagBranch "p" [] description
              : _
              ) <- tails content ]
    entries <- runSqlite "kcbrno_zpravy.sqlite" $ do
        runMigration migrateAll
        forM zpravy $ \(name, heading, description) -> do
            let title = TS.innerText $ TS.flattenTree heading
            let content = TS.renderTree description
            atomDate <- getZpravaLastUpdate name [title, content]
            let entryUri = "http://www.kcbrno.org/zpravy.php#" <> Text.unpack name
            let entryTitle = Text.unpack title
            let entryContent = mkAtomHtml $ Text.unpack content
            pure $ mkAtomEntry entryUri entryTitle atomDate entryContent
    pure $ mkAtomFeed feedUri "kcbrno.org zprávy" now entries
  where
    feedUri = "https://github.com/liskin/atomweb#kcBrnoZpravyFeed"

getZpravaLastUpdate :: Text.Text -> [Text.Text] -> SqlPersistM Date
getZpravaLastUpdate name content =
    getBy (ZpravaUniqueName name) >>= \case
        Nothing -> do
            now <- liftIO getCurrentAtomDate
            insert_ $ Zprava name sha now
            pure now
        Just zpravaEntity -> do
            let Zprava{..} = entityVal zpravaEntity
            if zpravaSha1 == sha
                then pure zpravaLastUpdate
                else do
                    now <- liftIO getCurrentAtomDate
                    update (entityKey zpravaEntity) [ZpravaLastUpdate =. now]
                    pure now
  where
    sha = showDigest $ sha1 $ Text.encodeUtf8 $ Text.intercalate "\RS" content

getTags :: String -> IO [TS.TagTree Text.Text]
getTags = (TS.parseTree . Text.decodeUtf8 . (^. Wreq.responseBody) <$>) . Wreq.get

_unused1 :: ZpravaId
_unused1 = _unused1
