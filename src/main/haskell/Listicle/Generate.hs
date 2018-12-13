{-# LANGUAGE RecordWildCards #-}

module Listicle.Generate
( stories
, image
, listicle
) where

import qualified Control.Applicative             as Applicative
import qualified Control.Monad                   as Monad
import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Class       as Trans
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Listicle.Util                   as Util
import qualified System.Random                   as Random

import Control.Applicative ((<|>))
import Control.Monad.Random (Rand)
import Control.Monad.Random (RandT)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

stories :: (RandomGen g)
        => Config
        -> RandT g Maybe [Story]
stories config@(Config { .. }) =
  let
    randomListicle :: (RandomGen g) => RandT g Maybe Text
    randomListicle  = listicle config =<< Util.randomFromSet configListicles 

    story          :: (RandomGen g) => RandT g Maybe Story
    story           = Applicative.liftA2 (\a b -> Story (a, b)) randomListicle (image configImageStore)

    (Params { .. }) = configParams
  in
    Monad.replicateM (fromIntegral paramsStoriesPerPage) story

image :: (RandomGen g)
      => ImageStore
      -> RandT g Maybe Image
image (ImageStore images) = Util.randomFromSet images

listicle :: (RandomGen g)
         => Config
         -> Listicle
         -> RandT g Maybe Text
listicle config@(Config { .. }) (Listicle parts) =
    fmap (Util.capitalizeFirst . Text.concat) (mapM (generatePart config) parts)

generatePart :: (RandomGen g)
             => Config
             -> ListiclePart
             -> RandT g Maybe Text
generatePart _               (NormalText t)   = Trans.lift (Just t)
generatePart (Config { .. }) Number           = number configParams
generatePart (Config { .. }) (FillIn fillIns) = do
    (FillInPath { .. }) <- Util.randomFromSet fillIns
    dictionary          <- Trans.lift (Map.lookup fipBase configDicts)
    fillIn dictionary fipAttr

number :: (RandomGen g)
       => Params
       -> RandT g Maybe Text
number (Params { .. }) = do
    randomNumber <- Random.getRandomR (paramsMinNumber, paramsMaxNumber)
    pure (Text.pack (show randomNumber))

fillIn :: (RandomGen g)
       => Dictionary
       -> Maybe Text
       -> RandT g Maybe Text
fillIn (Dictionary terms) attr = do
    (Term { .. }) <- Util.randomFromSet terms

    let generatedAttr = (\attr -> Map.lookup attr termAttrs) =<< attr
    let generatedBase = Just termBase

    generatedValue <- Trans.lift (generatedAttr <|> generatedBase)

    pure generatedValue
