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

newtype TemplateStories = TemplateStories [TemplateStory]

instance Mustache.ToMustache TemplateStories where
    toMustache (TemplateStories stories) = Mustache.object [ "stories" ~> stories ] 

newtype TemplateStory = TemplateStory Story

instance Mustache.ToMustache TemplateStory where
    toMustache (TemplateStory (Story (listicle, (Image { .. })))) = Mustache.object
      [ "title" ~> listicle
      , "image" ~> imgPath
      ] 

parse :: Text
      -> Either Text Template
parse = Bifunctor.bimap (Text.pack . show) Template . Mustache.compileTemplate "template"

render :: Template
       -> [Story]
       -> Text
render (Template template) stories =
    Mustache.substitute template (TemplateStories (map TemplateStory stories))
