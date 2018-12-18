{-# LANGUAGE OverloadedStrings #-}

module Listicle.Avatar
( generate
, load
) where

import qualified Control.Monad.Trans.Class       as Trans
import qualified Data.Text                       as Text
import qualified Listicle.Util                   as Util

import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array (Array)
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

generate :: (RandomGen g)
         => Array Int Text
         -> RandT g (Except Text) Text
generate = Util.randomFromArray

load :: FilePath
     -> ExceptT Text IO (Array Int Text)
load avatarsPath = do
    content <- Trans.lift (readFile avatarsPath)
    pure (Util.array (map Text.pack (lines content)))
