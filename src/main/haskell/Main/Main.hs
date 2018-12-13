{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Random            as Random
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import qualified Data.Maybe                      as Maybe
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Data.Traversable                as Traversable
import qualified Listicle.Generate               as Generate
import qualified Listicle.Parse.Listicle         as Listicle
import qualified Listicle.Parse.Dictionary       as Dictionary
import qualified Listicle.Util                   as Util
import qualified Paths_listicle                  as Cabal
import qualified System.Directory                as Directory
import qualified Web.Scotty                      as Scotty

import Data.Set (Set)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

resourcesDirectory :: Text
resourcesDirectory = "src/main/resources/"

defaultParams :: Params
defaultParams = Params { paramsMinNumber = 3, paramsMaxNumber = 30, paramsStoriesPerPage = 10 }

main :: IO ()
main = do
    dictionaryDir <- Cabal.getDataFileName (Text.unpack resourcesDirectory <> "dictionary")
    dicts         <- loadDictionaries dictionaryDir

    listiclesPath <- Cabal.getDataFileName (Text.unpack resourcesDirectory <> "listicles.list")
    listicles     <- loadListicles listiclesPath

    imagesDir     <- Cabal.getDataFileName (Text.unpack resourcesDirectory <> "static/img")
    imageStore    <- loadImageStore imagesDir

    let config     = Config {
        configParams     = defaultParams,
        configDicts      = dicts,
        configListicles  = listicles,
        configImageStore = imageStore }

    g             <- Random.getStdGen
    let results    = Maybe.fromMaybe [] (Random.evalRandT (Generate.stories config) g)

    mapM_ print results

--    store     <- Store.inMemory
--
--    Scotty.scotty 3000 $ do
--        --Scotty.get "/:word" $ do
--        --    beam <- Scotty.param "word"
--        --    Scotty.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--
--        Route.staticRoutes (resourcesDirectory <> "static/")
--    
--        let appConfig = Controller.AppConfig
--              { Controller.boardConfig = Create.BoardConfig
--                  { Create.boardHeight = 5
--                  , Create.boardWidth  = 5
--                  , Create.redWords    = 7
--                  , Create.blueWords   = 7
--                  , Create.assassins   = 1
--                  }
--              , Controller.dictionary = map Text.pack wordList
--              }
--    
--        Route.apiRoutes appConfig store

loadDictionaries :: FilePath
                 -> IO (Map Text Dictionary)
loadDictionaries dictionaryDir = do
    files <- Directory.getDirectoryContents dictionaryDir
    let dictFiles = filter (List.isSuffixOf ".json") files

    dictionaryList <- Traversable.forM dictFiles (\fileName -> do
        let name = Text.pack (List.take (length fileName - 5) fileName)

        content <- readFile (dictionaryDir <> "/" <> fileName)

        dict <- case Dictionary.parse (Text.pack content) of
            (Left error) -> fail (Text.unpack error)
            (Right dict) -> pure dict

        pure (name, dict))

    pure (Map.fromList dictionaryList)

loadListicles :: FilePath
              -> IO (Set Listicle)
loadListicles listiclesPath = do
    lines <- fmap lines (readFile listiclesPath)

    listicles <- Traversable.forM lines (\line -> case Listicle.parse (Text.pack line) of
        (Left error)     -> fail (Text.unpack error)
        (Right listicle) -> pure listicle)

    pure (Set.fromList listicles)

loadImageStore :: FilePath
               -> IO ImageStore
loadImageStore imageDir = do
    files <- Directory.getDirectoryContents imageDir

    let
      images =
        [ Image ("static/" <> (Text.pack fileName))
        | fileName <- files
        , fileName /= "." && fileName /= ".." ]

    pure (ImageStore (Set.fromList images))
