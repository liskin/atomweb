{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Bool (Bool(True), otherwise)
import Data.Function ((.), ($))
import System.Environment (getArgs)
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (read)

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

main :: IO ()
main = do
    [port] <- getArgs
    run (read port) . logger . gzip def $ app

logger :: Middleware
logger
    | True = logStdoutDev
    | otherwise = unsafePerformIO $ mkRequestLogger def{ outputFormat = Apache FromFallback }

app :: Request -> (Response -> IO b) -> IO b
app req respond = case (requestMethod req, pathInfo req) of
    (_, _) -> respond $ responseNotImplemented

responseNotImplemented :: Response
responseNotImplemented = responseBuilder status501 headers $ fromString "not implemented"
    where
        headers = [("Content-Type", "text/plain")]
