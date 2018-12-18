{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Config
( defaultParams
, load
, resourcesBaseDir
) where

import qualified Control.Monad.Trans.Except      as Except
import qualified Listicle.Avatar                 as Avatar
import qualified Listicle.Dictionary             as Dictionary
import qualified Listicle.ImageStore             as ImageStore
import qualified Listicle.Name                   as Name
import qualified Listicle.Phrase                 as Phrase

import Control.Monad.Trans.Except (ExceptT)
import Data.Array (Array)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)

import Listicle.Types

defaultParams :: Params
defaultParams = Params {
    paramsMinNumber = 3,
    paramsMaxNumber = 30,
    paramsStoriesPerPage = 120 }

resourcesBaseDir :: FilePath
resourcesBaseDir = "src/main/resources/"

load :: String
     -> ExceptT Text IO Config
load resourcesDir = do
    dicts         <- Dictionary.load (resourcesDir <> "dictionary")
    headlines     <- Phrase.load     (resourcesDir <> "headline.list")
    summaries     <- Phrase.load     (resourcesDir <> "summary.list")
    imageStore    <- ImageStore.load (resourcesDir <> "static/img")
    names         <- Name.load       (resourcesDir <> "name.list")
    avatars       <- Avatar.load     (resourcesDir <> "avatar.list")

    pure $ Config {
        configParams     = defaultParams,
        configDicts      = dicts,
        configHeadlines  = headlines,
        configSummaries  = summaries,
        configImageStore = imageStore,
        configNames      = names,
        configAvatars    = avatars
    }
