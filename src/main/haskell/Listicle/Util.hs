{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Util
( array
, capitalizeFirst
, randomFromArray
, rescue
, toExcept
) where

import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Array                      as Array
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Functor.Identity           as Identity
import qualified Data.Text                       as Text

import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Data.Array ((!))
import Data.Array (Array)
import Data.Maybe (Maybe)
import System.Random (RandomGen)
import Data.Text (Text)

randomFromArray :: (RandomGen g)
                => Array Int a
                -> RandT g (Except Text) a
randomFromArray arr =
  let
    bounds = Array.bounds arr
    size   =
        if snd bounds < fst bounds
        then 0
        else snd bounds + 1
  in
    if size > 0
    then do
      randomIndex <- Random.getRandomR (0, size - 1)
      pure (arr ! randomIndex)
    else Trans.lift (Except.throwE "Can't randomly select from empty array")

array :: [a] -> Array Int a
array l = Array.listArray (0, length l - 1) l

capitalizeFirst :: Text
                -> Text
capitalizeFirst input =
  let
    (firstLetter, rest) = Text.splitAt 1 input
  in
    Text.toTitle firstLetter <> rest

toExcept :: a -> Maybe b -> Except a b
toExcept _ (Just b) = pure b
toExcept a Nothing  = Except.throwE a

rescue :: b -> Maybe b -> Except a b
rescue b Nothing  = pure b
rescue _ (Just b) = pure b
