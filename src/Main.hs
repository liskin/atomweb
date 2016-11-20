{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Bool (Bool(True, False))
import Data.Function ((.), ($))
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Default (def)
import Network.HTTP.Types (status501)
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
    (_, _) -> respond $ responseNotImplemented

responseNotImplemented :: Response
responseNotImplemented = responseBuilder status501 headers $ fromString "not implemented"
    where
        headers = [("Content-Type", "text/plain")]
