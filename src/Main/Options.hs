{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main.Options
    ( Opts(..)
    , getOpts
    )
  where

import Control.Applicative ((<$>), (<*>))
import Data.Bool (Bool)
import Data.Function (($))
import Data.Monoid ((<>))
import System.IO (IO)

import Options.Applicative
    ( Parser
    , auto
    , execParser
    , fullDesc
    , help
    , helper
    , info
    , long
    , option
    , short
    , switch
    )
import Network.Wai.Handler.Warp (Port)

data Opts = Opts
    { port :: Port
    , dev :: Bool
    }

getOpts :: IO Opts
getOpts = execParser $ info (helper <*> opts) fullDesc

opts :: Parser Opts
opts = Opts
    <$> option auto (long "port" <> short 'p' <> help "port")
    <*> switch (long "dev" <> help "dev mode (logging)")
