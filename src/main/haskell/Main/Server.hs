{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import qualified Data.Text                       as Text
import qualified Listicle.Load                   as Load
import qualified Listicle.Web.Route              as Route
import qualified Paths_listicle                  as Cabal
import qualified Web.Scotty                      as Scotty

import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

main :: IO ()
main = do
    resourcesDir <- Cabal.getDataFileName Load.resourcesBaseDir
    config       <- Load.config resourcesDir

    startWebServer config resourcesDir 3000

startWebServer :: Config
               -> String
               -> Int
               -> IO ()
startWebServer config resourcesDir port = Scotty.scotty port $ do
    Route.withSeed config
    Route.generateSeed
    Route.static (Text.pack resourcesDir <> "static/")

