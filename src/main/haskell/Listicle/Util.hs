{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Util
( randomFromSet
, capitalizeFirst
) where

import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Class       as Trans
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text

import Control.Monad.Random (RandT)
import Data.Maybe (Maybe)
import Data.Set (Set)
import System.Random (RandomGen)
import Data.Text (Text)

randomFromSet :: (RandomGen g)
              => Set a
              -> RandT g Maybe a
randomFromSet set =
  let
    size = Set.size set
  in
    if size > 0
    then do
      randomIndex <- Random.getRandomR (0, size - 1)
      pure (Set.elemAt randomIndex set)
    else Trans.lift Nothing

capitalizeFirst :: Text
                -> Text
capitalizeFirst input =
  let
    (firstLetter, rest) = Text.splitAt 1 input
  in
    Text.toTitle firstLetter <> rest
