{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
( main
) where

import qualified Control.Monad                   as Monad
import qualified Control.Monad.Morph             as Morph
import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Functor.Identity           as Identity
import qualified Data.Maybe                      as Maybe
import qualified Data.Text                       as Text
import qualified Listicle.Config                 as Config
import qualified Listicle.News                   as News
import qualified Paths_listicle                  as Cabal
import qualified System.Environment              as Environment

import Control.Monad.Trans.Except (ExceptT)
import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

main :: IO ()
main = do
    resourcesDir <- Cabal.getDataFileName Config.resourcesBaseDir
    [n]          <- Environment.getArgs

    result       <- Except.runExceptT $ do
        config <- Config.load resourcesDir
        printNListicles $ config {
            configParams = (configParams config) {
                paramsStoriesPerPage = read n
            }
        }

    case result of
        (Right ())   -> pure ()
        (Left error) -> fail (Text.unpack error)


printNListicles :: Config
                -> ExceptT Text IO ()
printNListicles config = do
    g                <- Trans.lift Random.getStdGen
    let generatedPage = Random.evalRandT (News.generatePage config) g
    (Page {..})      <- Morph.hoist (pure . Identity.runIdentity) generatedPage

    Trans.lift $ Monad.forM_ pageMainStories $ \(Story { .. }) ->
        putStrLn (Text.unpack storyHeadline)
