{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (join)
import System.IO.Unsafe (unsafePerformIO)

import Data.Default (def)
import Data.Text (Text)
import Network.HTTP.Types (status200, status404, status501)
import Network.Wai
    ( Middleware
    , Request(requestMethod)
    , Response
    , pathInfo
    , responseBuilder
    , queryString
    )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger
    ( IPAddrSource(FromFallback)
    , OutputFormat(Apache)
    , logStdoutDev
    , mkRequestLogger
    , outputFormat
    )
import Text.Feed.Constructor (getFeedKind)
import Text.Feed.Export (textFeed)
import Text.Feed.Types (Feed, FeedKind(AtomKind))

import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import Main.Feed.Example (exampleFeed)
import Main.Feed.KCBrno (kcBrnoDiskuzeFeed, kcBrnoZpravyFeed)
import Main.Feed.Reddit (subRedditFeed)
import Main.Options (Opts(..), getOpts)

main :: IO ()
main = do
    Opts{..} <- getOpts
    run port . logger dev . gzip def $ app

logger :: Bool -> Middleware
logger True = logStdoutDev
logger False = unsafePerformIO $ mkRequestLogger def{ outputFormat = Apache FromFallback }

app :: Request -> (Response -> IO b) -> IO b
app req respond = case (requestMethod req, pathInfo req) of
    ("GET", ["example.atom"]) -> exampleFeed >>= respond . responseFeed
    ("GET", ["kcbrno_diskuze.atom"]) -> kcBrnoDiskuzeFeed >>= respond . responseFeed
    ("GET", ["kcbrno_zpravy.atom"]) -> kcBrnoZpravyFeed >>= respond . responseFeed
    ("GET", ["subreddit.atom", r]) -> do
        let q = queryString' req
        let minScore = fmap (read . T.unpack) . join $
                "minScore" `lookup` q
        feed <- subRedditFeed r minScore
        respond $ responseFeed feed
    ("GET", _) -> respond $ responseNotFound
    (_, _) -> respond $ responseNotImplemented

queryString' :: Request -> [(Text, Maybe Text)]
queryString' = fmap toText . queryString
    where
        toText (a, b) = (T.decodeUtf8 a, fmap T.decodeUtf8 b)

responseFeed :: Feed -> Response
responseFeed feed = responseBuilder status200 headers response
  where
    response = Builder.fromLazyByteString $ TL.encodeUtf8 xmlText
    Just xmlText = textFeed feed
    headers = case getFeedKind feed of
        AtomKind -> [("Content-Type", "application/atom+xml")]
        _ -> error "unknown feed kind"

responseNotFound :: Response
responseNotFound = responseBuilder status404 headers response
    where
        response = Builder.fromByteString "not found"
        headers = [("Content-Type", "text/plain")]

responseNotImplemented :: Response
responseNotImplemented = responseBuilder status501 headers response
    where
        response = Builder.fromByteString "not implemented"
        headers = [("Content-Type", "text/plain")]
