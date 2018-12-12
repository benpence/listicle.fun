{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Listicle.Types where

import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import System.Random (RandomGen)

data Dictionary
    = Dictionary (Set Term)
    deriving (Eq, Show, Ord, Generic)

data Term
    = Term
      { termBase  :: Text
      , termAttrs :: Map Text Text
      }
    deriving (Eq, Show, Ord, Generic)

data Config
    = Config
      { configMaxNumber :: Integer
      }

data Listicle
    = Listicle [ListiclePart]
    deriving (Eq, Show, Ord, Generic)

data ListiclePart
    = NormalText Text
    | Number
    | FillIn (Set FillInPath)
    deriving (Eq, Show, Ord, Generic)

data FillInPath
    = FillInPath
      { fipBase :: Text
      , fipAttr :: Maybe Text
      }
    deriving (Eq, Show, Ord, Generic)
