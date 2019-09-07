{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Feed.Reddit
    ( subRedditFeed
    )
  where

import Control.Lens ((^.))
import Control.Monad (guard)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Exts (toList)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed(AtomFeed))

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.Wreq as Wreq
import qualified Text.Atom.Feed as Atom

type RedditData = (Feed, Map Text J.Object)

subRedditFeed :: Text -> Maybe Int -> IO Feed
subRedditFeed sub minScore =
    fst . maybe id filterByMinScore minScore <$> fetchReddit sub

fetchReddit :: Text -> IO RedditData
fetchReddit sub = do
    feed <- fetchRedditAtom sub
    info <- jsonEntries <$> fetchRedditJson sub
    pure (feed, info)

fetchRedditAtom :: Text -> IO Feed
fetchRedditAtom = (parse <$>) . Wreq.get . T.unpack . mkUri
    where
        mkUri sub = "https://www.reddit.com/r/" <> sub <> "/top/.rss?limit=100&sort=top&t=week"
        parse = maybe err id . parseFeedSource . (^. Wreq.responseBody)
        err = error "can't parse reddit feed"

fetchRedditJson :: Text -> IO J.Value
fetchRedditJson = (parse <$>) . Wreq.get . T.unpack . mkUri
    where
        mkUri sub = "https://www.reddit.com/r/" <> sub <> "/top.json?limit=100&raw_json=1&sort=top&t=week"
        parse = maybe err id . J.decodeStrict' . BL.toStrict . (^. Wreq.responseBody)
        err = error "can't parse reddit json"

mapMaybeAtomEntries :: (Atom.Entry -> Maybe Atom.Entry) -> Feed -> Feed
mapMaybeAtomEntries f (AtomFeed atom) = AtomFeed atom'
    where
        atom' = atom{ Atom.feedEntries = mapMaybe f (Atom.feedEntries atom) }
mapMaybeAtomEntries _ _ = error "atom feed expected"

jsonEntries :: J.Value -> Map Text J.Object
jsonEntries = parseJsonValue parse
    where
        parse = J.withObject "root" $ \root ->
            root J..: "data" >>= parseData

        parseData = J.withObject "data" $ \data_ ->
            data_ J..: "children" >>= parseChildren

        parseChildren = J.withArray "children" $ \children ->
            M.fromList . toList <$> traverse parseEntry children

        parseEntry = J.withObject "entry" $ \entry ->
            entry J..: "data" >>= parseEntryData

        parseEntryData = J.withObject "entry data" $ \data_ -> do
            name <- data_ J..: "name"
            pure (name, data_)

parseJsonValue :: (j -> J.Parser a) -> j -> a
parseJsonValue parser = either error id . J.parseEither parser

filterByMinScore :: Int -> RedditData -> RedditData
filterByMinScore minScore (feed, info) = (mapMaybeAtomEntries f feed, info)
    where
        f entry@Atom.Entry{Atom.entryId} = do
            entryInfo <- entryId `M.lookup` info
            score <- J.parseMaybe (J..: "score") entryInfo
            guard $ (score :: Int) >= minScore
            pure entry
