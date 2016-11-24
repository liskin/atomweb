{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Prelude (error)

import Control.Monad ((>>=))
import Data.Bool (Bool(True, False))
import Data.Function ((.), ($))
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Default (def)
import Network.HTTP.Types (status200, status404, status501)
import Network.Wai (Middleware, Request(requestMethod), Response, pathInfo, responseBuilder)
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
import Text.Feed.Export (xmlFeed)
import Text.Feed.Types (Feed, FeedKind(AtomKind))
import Text.XML.Light.Output (showTopElement)

import Main.Feed.Example (exampleFeed)
import Main.Feed.KCBrno (kcBrnoDiskuzeFeed, kcBrnoZpravyFeed)
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
    ("GET", _) -> respond $ responseNotFound
    (_, _) -> respond $ responseNotImplemented

responseFeed :: Feed -> Response
responseFeed feed = responseBuilder status200 headers $ fromString xml
  where
    xml = showTopElement $ xmlFeed feed
    headers = case getFeedKind feed of
        AtomKind -> [("Content-Type", "application/atom+xml")]
        _ -> error "unknown feed kind"

responseNotFound :: Response
responseNotFound = responseBuilder status404 headers $ fromString "not found"
    where
        headers = [("Content-Type", "text/plain")]

responseNotImplemented :: Response
responseNotImplemented = responseBuilder status501 headers $ fromString "not implemented"
    where
        headers = [("Content-Type", "text/plain")]
