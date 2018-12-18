{-# LANGUAGE OverloadedStrings #-}

module Listicle.Name
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
load namesPath = do
    content <- Trans.lift (readFile namesPath)
    pure (Util.array (map Text.pack (lines content)))
