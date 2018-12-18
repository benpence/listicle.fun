{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.News
( generatePage
, generateStory
) where

import qualified Control.Monad                   as Monad
import qualified Listicle.Avatar                 as Avatar
import qualified Listicle.ImageStore             as ImageStore
import qualified Listicle.Name                   as Name
import qualified Listicle.Phrase                 as Phrase

import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

generatePage :: (RandomGen g)
              => Config
              -> RandT g (Except Text) Page
generatePage config@(Config { .. }) =
  let
    (Params { .. }) = configParams
  in do
    mainStories <- generateN (generateStory config) paramsStoriesPerPage
    sideStories <- generateN (generateStory config) paramsStoriesPerPage

    pure $ Page {
        pageMainStories = mainStories,
        pageSideStories = sideStories }

generateStory :: (RandomGen g)
              => Config
              -> RandT g (Except Text) Story
generateStory config@(Config { .. }) = do
    headline <- Phrase.generate config configHeadlines
    summary  <- Phrase.generate config configSummaries
    image    <- ImageStore.generate configImageStore
    author   <- Name.generate configNames
    avatar   <- Name.generate configAvatars

    pure $ Story {
        storyHeadline = headline,
        storySummary  = summary,
        storyImage    = image,
        storyAuthor   = author,
        storyAvatar   = avatar
    }

generateN :: (RandomGen g)
          => RandT g (Except Text) a
          -> Integer
          -> RandT g (Except Text) [a]
generateN gen n = Monad.replicateM (fromIntegral n) gen
