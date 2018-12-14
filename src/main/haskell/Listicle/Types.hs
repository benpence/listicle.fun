{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Listicle.Types where

import qualified Text.Mustache                   as Mustache

import Data.Array (Array)
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)
import System.Random (RandomGen)

data Config
    = Config
      { configParams     :: Params
      , configDicts      :: Map Text Dictionary
      , configListicles  :: Array Int Listicle
      , configImageStore :: ImageStore
      }
    deriving (Eq, Show, Ord, Generic)

data Params
    = Params
      { paramsMinNumber      :: Integer
      , paramsMaxNumber      :: Integer
      , paramsStoriesPerPage :: Integer
      }
    deriving (Eq, Show, Ord, Generic)


data Dictionary
    = Dictionary (Array Int Term)
    deriving (Eq, Show, Ord, Generic)

data Term
    = Term
      { termBase  :: Text
      , termAttrs :: Map Text Text
      }
    deriving (Eq, Show, Ord, Generic)


data Story
    = Story (Text, Image)
    deriving (Eq, Show, Ord, Generic)

data Listicle
    = Listicle [ListiclePart]
    deriving (Eq, Show, Ord, Generic)

data ListiclePart
    = NormalText Text
    | Number
    | FillIn (Array Int FillInPath)
    deriving (Eq, Show, Ord, Generic)

data FillInPath
    = FillInPath
      { fipBase :: Text
      , fipAttr :: Maybe Text
      }
    deriving (Eq, Show, Ord, Generic)


data ImageStore
    = ImageStore (Array Int Image)
    deriving (Eq, Show, Ord, Generic)

data Image
    = Image
      { imgPath :: Text
      }
    deriving (Eq, Show, Ord, Generic)
