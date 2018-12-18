{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Template
( generate
, load
, parse
, render
, Template
) where

import qualified Control.Monad.Morph             as Morph
import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Functor.Identity           as Identity
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Data.Traversable                as Traversable
import qualified Listicle.Util                   as Util
import qualified System.Directory                as Directory
import qualified Text.Mustache                   as Mustache

import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array (Array)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random (RandomGen)
import Text.Mustache ((~>))

import Listicle.Types

data Template
    = Template Mustache.Template
    deriving (Show)

generate :: (RandomGen g)
         => Array Int Template
         -> RandT g (Except Text) Template
generate templates = Util.randomFromArray templates

load :: FilePath
     -> ExceptT Text IO (Array Int Template)
load templateDir = do
    files <- Trans.lift (Directory.getDirectoryContents templateDir)
    let templateFiles = filter (List.isSuffixOf ".tpl") files

    templateList <- Traversable.forM templateFiles $ \fileName -> do
        content <- Trans.lift (readFile (templateDir <> "/" <> fileName))

        Morph.hoist (pure . Identity.runIdentity) (parse (Text.pack content))

    pure (Util.array templateList)


parse :: Text
      -> Except Text Template
parse = 
  let
    prepare = Except.except . Bifunctor.bimap (Text.pack . show) Template
  in
    prepare . Mustache.compileTemplate "template"


render :: Template
       -> Page
       -> Text
render (Template template) (Page { .. }) =
  let
    toTemplateStory (Story { .. }) = TemplateStory {
        tsHeadline = storyHeadline,
        tsSummary  = storySummary,
        tsImage    = storyImage,
        tsAuthor   = storyAuthor,
        tsAvatar   = storyAvatar
    }
  in
    Mustache.substitute template $ TemplatePage {
        tpMainStories = map toTemplateStory pageMainStories,
        tpSideStories = map toTemplateStory pageSideStories }

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

data TemplateStory
    = TemplateStory
      { tsHeadline :: Text
      , tsSummary  :: Text
      , tsImage    :: Image
      , tsAuthor   :: Text
      , tsAvatar   :: Text
      }
    deriving (Eq, Show, Ord, Generic)

instance Mustache.ToMustache TemplateStory where
    toMustache (TemplateStory { .. }) = Mustache.object
      [ "headline"  ~> tsHeadline
      , "summary"   ~> tsSummary 
      , "imagePath" ~> imgPath tsImage
      , "author"    ~> tsAuthor
      , "avatar"    ~> tsAvatar
      ]
