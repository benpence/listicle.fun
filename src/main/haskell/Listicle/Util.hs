{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Util
( randomFromSet
, capitalizeFirst
, sequenceRandomGen
) where

import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Set                        as Set
import qualified System.Random                   as Random
import qualified Data.Text                       as Text

import Data.Maybe (Maybe)
import Data.Set (Set)
import System.Random (RandomGen)
import Data.Text (Text)

randomFromSet :: (RandomGen g)
              => Set a
              -> g
              -> Maybe (a, g)
randomFromSet set g = 
  let
    size              = Set.size set
    (randomIndex, g') = Random.randomR (0, size - 1) g
  in
    if size > 0
    then Just (Set.elemAt randomIndex set, g')
    else Nothing

capitalizeFirst :: Text
                -> Text
capitalizeFirst input =
  let
    (firstLetter, rest) = Text.splitAt 1 input
  in
    Text.toTitle firstLetter <> rest

sequenceRandomGen :: RandomGen g
                  => [g -> (a, g)]
                  -> g
                  -> ([a], g)
sequenceRandomGen fs g =
  let
    step (as, g) f = Bifunctor.first (: as) (f g) 
  in
    Bifunctor.first reverse (foldl step ([], g) fs)
