{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Feed.Reddit
    ( subRedditFeed
    )
  where

import Control.Applicative (pure)
import Control.Monad ((<=<), fail)
import Data.Either (either)
import Data.Function ((.), ($), id)
import Data.Int (Int)
import Data.List (map, filter)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Prelude ((>=), fromIntegral)
import System.IO (IO)
import Text.Show (show)

import qualified Data.ByteString.Char8 as B (pack)
import Data.Default (def)
import qualified Data.Text as T
import Reddit
    ( Listing(..)
    , Post
    , RedditOptions(..)
    , RedditT
    , SubredditName(..)
    , getPosts'
    , runRedditWith
    )
import qualified Reddit.Types.Post as Post
    ( Post(..)
    , PostContent(..)
    )
import qualified Reddit.Types.Listing as Listing (ListingType(..))

import Paths_atomweb (version)
import Main.Feed
    ( Entry
    , Feed
    , getCurrentAtomDate
    , mkAtomDate
    , mkAtomEntry
    , mkAtomFeed
    , mkAtomHtml
    )

getPosts :: SubredditName -> IO [Post]
getPosts r = runRedditAnon' $ do
    Listing _ _ posts <- getPosts' def Listing.Hot (Just r)
    pure posts

runRedditAnon' :: RedditT IO a -> IO a
runRedditAnon' = either (fail . show) pure <=< runRedditWith o
    where
        o = def{ customUserAgent = Just (B.pack ua) }
        ua = "atomweb-" <> showVersion version

postToEntry :: Post -> Entry
postToEntry p = mkAtomEntry url title date content
    where
        url = T.unpack $ "https://www.reddit.com" <> Post.permalink p
        title = T.unpack $ Post.title p
        date = mkAtomDate $ Post.created p
        content = mkAtomHtml $ case Post.content p of
            Post.SelfPost _ html -> T.unpack html
            _ -> ""

minScoreFilter :: Int -> [Post] -> [Post]
minScoreFilter minScore = filter $ \p ->
    Post.score p >= fromIntegral minScore

subRedditFeed :: T.Text -> Maybe Int -> IO Feed
subRedditFeed r minScore = do
    now <- getCurrentAtomDate
    posts <- getPosts (R r)
    let postFilter = maybe id minScoreFilter minScore
    let entries = map postToEntry . postFilter $ posts
    let feedId = "https://github.com/liskin/atomweb#subRedditFeed_" <> T.unpack r
    pure $ mkAtomFeed feedId (T.unpack r) now entries
