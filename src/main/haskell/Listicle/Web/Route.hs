{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Web.Route
  ( generateSeed
  , static
  , withSeed
  ) where

import qualified Control.Monad.IO.Class          as IO
import qualified Control.Monad.Random            as Random
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as LazyText
import qualified Listicle.Generate               as Generate
import qualified Paths_listicle                  as Cabal
import qualified System.Random                   as Random
import qualified Web.Scotty                      as Scotty

import Control.Monad.Random (Rand)
--import Data.Monoid ((<>))
import Data.Text (Text)
--import Text.Read (readMaybe)
import System.Random (RandomGen)

import Listicle.Types

withSeed :: Config
         -> Scotty.ScottyM ()
withSeed config@(Config { .. }) = do
    Scotty.get (Scotty.regex "^/([0-9]{1,9})$") $ do
        seed <- fmap (read . Text.unpack) (Scotty.param "1")
        
        let g            = (Random.mkStdGen seed) 
        let maybeStories = Random.evalRandT (Generate.stories config) g

        IO.liftIO (print maybeStories)

generateSeed :: Scotty.ScottyM ()
generateSeed =
  let
    rng :: (RandomGen g) => Rand g Int
    rng = Random.getRandomR (1, 999999999)
  in do
    Scotty.get "/" $ do
        randomNumber <- IO.liftIO (Random.evalRandIO rng)
        Scotty.redirect ("/" <> LazyText.pack (show randomNumber))

-- | Add routes for routes the browser might visit directly.
static :: Text -> Scotty.ScottyM ()
static staticDirectory = do
    -- | Serve static files from a directory
    serveStaticDirectory "/static/" staticDirectory

serveStaticDirectory :: Text -> Text -> Scotty.ScottyM ()
serveStaticDirectory staticRouteDirectory staticDirectory = do
    let routePath = Text.unpack ("^" <> staticRouteDirectory <> "(.*)$")

    Scotty.get (Scotty.regex routePath) $ do
        path <- Scotty.param "1"
        let relativePath = staticDirectory <> path
        absolutePath <- IO.liftIO (Cabal.getDataFileName (Text.unpack relativePath))

        Scotty.file absolutePath
