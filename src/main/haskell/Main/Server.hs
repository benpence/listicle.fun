{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Text                       as Text
import qualified Listicle.Config                 as Config
import qualified Listicle.Template               as Template
import qualified Listicle.Web.Route              as Route
import qualified Paths_listicle                  as Cabal
import qualified Web.Scotty                      as Scotty

import Data.Array (Array)
import Data.Monoid ((<>))
import Data.Text (Text)
import Listicle.Template (Template)

import Listicle.Types

main :: IO ()
main = do
    resourcesDir <- Cabal.getDataFileName Config.resourcesBaseDir
    result       <- Except.runExceptT $ do
        config    <- Config.load resourcesDir
        templates <- Template.load (resourcesDir <> "template")
        Trans.lift $ startWebServer config templates resourcesDir 3000

    case result of
        (Right ())   -> pure ()
        (Left error) -> fail (Text.unpack error)

startWebServer :: Config
               -> Array Int Template
               -> FilePath
               -> Int
               -> IO ()
startWebServer config templates resourcesDir port = Scotty.scotty port $ do
    Route.withSeed config templates
    Route.generateSeed
    Route.static (Text.pack resourcesDir <> "static/")

