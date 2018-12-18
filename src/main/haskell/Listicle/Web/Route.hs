{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Web.Route
( generateSeed
, static
, withSeed
) where

import qualified Control.Monad.IO.Class          as IO
import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as LazyText
import qualified Listicle.Util                   as Util 
import qualified Listicle.News                   as News 
import qualified Listicle.Template               as Template
import qualified Paths_listicle                  as Cabal
import qualified System.Random                   as Random
import qualified Web.Scotty                      as Scotty

import Control.Monad.Random (Rand)
import Data.Array (Array)
import Data.Text (Text)
import Listicle.Template (Template)
import System.Random (RandomGen)

import Listicle.Types

withSeed :: Config
         -> Array Int Template
         -> Scotty.ScottyM ()
withSeed config@(Config { .. }) templates = do
    Scotty.get (Scotty.regex "^/([0-9]{1,9})$") $ do
        seed <- fmap (read . Text.unpack) (Scotty.param "1")
        
        let
            generateHtml = do
                page     <- News.generatePage config
                template <- Template.generate templates

                pure (Template.render template page)

        case Except.runExcept (Random.evalRandT generateHtml (Random.mkStdGen seed)) of
            (Right html) -> Scotty.html (LazyText.fromStrict html)
            (Left error) -> Scotty.html (LazyText.fromStrict error)

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
static staticDir = do
    -- | Serve static files from a directory
    serveStaticDirectory "/static/" staticDir

serveStaticDirectory :: Text -> Text -> Scotty.ScottyM ()
serveStaticDirectory staticRouteDir staticDir = do
    let routePath = Text.unpack ("^" <> staticRouteDir <> "(.*)$")

    Scotty.get (Scotty.regex routePath) $ do
        path <- Scotty.param "1"
        Scotty.file (Text.unpack staticDir <> path)
