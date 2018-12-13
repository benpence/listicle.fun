{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import qualified Control.Monad.Random            as Random
import qualified Control.Monad                   as Monad
import qualified Data.Maybe                      as Maybe
import qualified Data.Text                       as Text
import qualified Listicle.Load                   as Load
import qualified Listicle.Generate               as Generate
import qualified Paths_listicle                  as Cabal
import qualified System.Environment              as Environment

import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

main :: IO ()
main = do
    resourcesDir <- Cabal.getDataFileName Load.resourcesBaseDir
    config       <- Load.config resourcesDir

    [n]          <- Environment.getArgs

    printNListicles (config { configParams = (configParams config) { paramsStoriesPerPage = read n }})

printNListicles :: Config
                -> IO ()
printNListicles config = do
    g             <- Random.getStdGen
    let results    = Maybe.fromMaybe [] (Random.evalRandT (Generate.stories config) g)

    Monad.forM_ results (\(Story (listicle, _)) -> putStrLn (Text.unpack listicle))
