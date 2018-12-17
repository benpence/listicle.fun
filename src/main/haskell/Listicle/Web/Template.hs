{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Web.Template where
-- ( parse
-- , render
-- , Template
-- ) where

import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Text                       as Text
import qualified Text.Mustache                   as Mustache

import Data.Text (Text)
import Text.Mustache ((~>))

import Listicle.Types

data Template
    = Template Mustache.Template
    deriving (Show)

data TemplatePage
    = TemplatePage
      { tpMainStories :: [TemplateStory]
      , tpSideStories :: [TemplateStory]
      }

instance Mustache.ToMustache TemplatePage where
    toMustache (TemplatePage { .. }) = Mustache.object
      [ "mainStories" ~> tpMainStories
      , "sideStories" ~> tpSideStories
      ] 

newtype TemplateStory = TemplateStory Story

instance Mustache.ToMustache TemplateStory where
    toMustache (TemplateStory (Story (headline, (Image { .. })))) = Mustache.object
      [ "title" ~> headline
      , "image" ~> imgPath
      ] 

parse :: Text
      -> Either Text Template
parse = Bifunctor.bimap (Text.pack . show) Template . Mustache.compileTemplate "template"

render :: Template
       -> [Story]
       -> [Story]
       -> Text
render (Template template) mainStories sideStories =
    Mustache.substitute template $ TemplatePage {
        tpMainStories = map TemplateStory mainStories,
        tpSideStories = map TemplateStory sideStories }
 
