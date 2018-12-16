{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Load
( config
, defaultParams
, dicts
, imageStore
, listicles
, resourcesBaseDir
, templates
) where

import qualified Data.List                       as List
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Data.Traversable                as Traversable
import qualified Listicle.Parse                  as Parse
import qualified Listicle.Util                   as Util
import qualified Listicle.Web.Template           as Template
import qualified System.Directory                as Directory

import Data.Array (Array)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Listicle.Web.Template (Template)

import Listicle.Types

defaultParams :: Params
defaultParams = Params { paramsMinNumber = 3, paramsMaxNumber = 30, paramsStoriesPerPage = 40 }

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
            (Left error) -> ioError (userError (Text.unpack error))
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

    pure (ImageStore (Util.array images))

listicles :: FilePath
          -> IO (Array Int Listicle)
listicles listiclesPath = do
    lines <- fmap lines (readFile listiclesPath)

    listicleList <- Traversable.forM lines (\line -> case Parse.listicle (Text.pack line) of
        (Left error)     -> fail (Text.unpack error)
        (Right listicle) -> pure listicle)

    pure (Util.array listicleList)

templates :: FilePath
         -> IO (Array Int Template)
templates templateDir = do
    files <- Directory.getDirectoryContents templateDir
    let templateFiles = filter (List.isSuffixOf ".tpl") files

    templateList <- Traversable.forM templateFiles (\fileName -> do
        content <- readFile (templateDir <> "/" <> fileName)

        case Template.parse (Text.pack content) of
            (Left error) -> fail (Text.unpack error)
            (Right template) -> pure template
        )

    pure (Util.array templateList)
