{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Generate.Listicle where
-- ( generate 
-- ) where

import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Listicle.Util                   as Util
import qualified System.Random                   as Random

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

generate :: (RandomGen g)
         => Config
         -> Map Text Dictionary
         -> Listicle
         -> g
         -> Maybe ([Text], g)
generate config dicts (Listicle parts) g =
  let
    sequenceRandom :: (RandomGen g)
                   => Maybe ([Text], g)
                   -> ListiclePart
                   -> Maybe ([Text], g)
    sequenceRandom Nothing            _    = Nothing
    sequenceRandom (Just (texts, g')) part = do
        (text, g'') <- generatePart config dicts part g'
        pure (text : texts, g'')
  in
    fmap (Bifunctor.first reverse) (foldl sequenceRandom (Just ([], g)) parts)

generatePart :: (RandomGen g)
             => Config
             -> Map Text Dictionary
             -> ListiclePart
             -> g
             -> Maybe (Text, g)
generatePart _      _     (NormalText t)   g = Just (t, g)
generatePart config _     Number           g = Just (number config g)
generatePart _      dicts (FillIn fillIns) g = do
    ((FillInPath { .. }),  g') <- Util.randomFromSet fillIns g
    dictionary                 <- Map.lookup fipBase dicts
    fillIn dictionary fipAttr g'

number :: (RandomGen g)
       => Config
       -> g
       -> (Text, g)
number (Config { .. }) g = 
  let
    (randomNumber, g') = Random.randomR (1, configMaxNumber) g
  in
    ((Text.pack (show randomNumber)), g')

fillIn :: (RandomGen g)
       => Dictionary
       -> Maybe Text
       -> g
       -> Maybe (Text, g)
fillIn (Dictionary terms) attr g = do
    ((Term { .. }), g') <- Util.randomFromSet terms g

    let generatedAttr = (\attr -> Map.lookup attr termAttrs) =<< attr
    let generatedBase = Just termBase

    generatedValue <- generatedAttr <|> generatedBase

    pure (generatedValue, g')
