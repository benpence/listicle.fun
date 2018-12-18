{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.ImageStore
( load
, generate
) where

import qualified Control.Monad.Trans.Class       as Trans
import qualified Data.Text                       as Text
import qualified Listicle.Util                   as Util
import qualified System.Directory                as Directory

import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

generate :: (RandomGen g)
         => ImageStore
         -> RandT g (Except Text) Image
generate (ImageStore images) = Util.randomFromArray images

load :: FilePath
     -> ExceptT Text IO ImageStore
load imageDir = do
    files <- Trans.lift (Directory.getDirectoryContents imageDir)

    let
      images =
        [ Image ("/static/img/" <> (Text.pack fileName))
        | fileName <- files
        , fileName /= "." && fileName /= ".." ]

    pure (ImageStore (Util.array images))
