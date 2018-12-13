{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Load
( config
, defaultParams
, dicts
, imageStore
, listicles
, resourcesBaseDir
) where

import qualified Data.List                       as List
import qualified Listicle.Parse                  as Parse
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Data.Traversable                as Traversable
import qualified System.Directory                as Directory

import Data.Set (Set)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

defaultParams :: Params
defaultParams = Params { paramsMinNumber = 3, paramsMaxNumber = 30, paramsStoriesPerPage = 10 }

resourcesBaseDir :: FilePath
resourcesBaseDir = "src/main/resources/"

config :: String
       -> IO Config
config resourcesDir = do
    dicts         <- dicts (resourcesDir <> "dictionary")
    listicles     <- listicles (resourcesDir <> "listicles.list")
    imageStore    <- imageStore (resourcesDir <> "static/img")

    pure $ Config {
        configParams     = defaultParams,
        configDicts      = dicts,
        configListicles  = listicles,
        configImageStore = imageStore }

dicts :: FilePath
      -> IO (Map Text Dictionary)
dicts dictionaryDir = do
    files <- Directory.getDirectoryContents dictionaryDir
    let dictFiles = filter (List.isSuffixOf ".json") files

    dictionaryList <- Traversable.forM dictFiles (\fileName -> do
        let name = Text.pack (List.take (length fileName - 5) fileName)

        content <- readFile (dictionaryDir <> "/" <> fileName)

        dict <- case Parse.dictionary (Text.pack content) of
            (Left error) -> fail (Text.unpack error)
            (Right dict) -> pure dict

        pure (name, dict))

    pure (Map.fromList dictionaryList)

imageStore :: FilePath
           -> IO ImageStore
imageStore imageDir = do
    files <- Directory.getDirectoryContents imageDir

    let
      images =
        [ Image ("/static/img/" <> (Text.pack fileName))
        | fileName <- files
        , fileName /= "." && fileName /= ".." ]

    pure (ImageStore (Set.fromList images))

listicles :: FilePath
          -> IO (Set Listicle)
listicles listiclesPath = do
    lines <- fmap lines (readFile listiclesPath)

    listicleList <- Traversable.forM lines (\line -> case Parse.listicle (Text.pack line) of
        (Left error)     -> fail (Text.unpack error)
        (Right listicle) -> pure listicle)

    pure (Set.fromList listicleList)
