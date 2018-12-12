{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Parse.Dictionary
( parse
) where

import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as Aeson
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Encoding

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Listicle.Types

newtype ParsedDictionary
    = ParsedDictionary (Map Text (Map Text Text))
    deriving (Eq, Show, Ord, Generic)

instance Aeson.FromJSON ParsedDictionary

parse :: Text -> Either Text Dictionary
parse =
  let
    prettyException = Bifunctor.first Text.pack
    convertDict     = fmap toDictionary
  in
    convertDict . prettyException . Aeson.eitherDecode . ByteString.fromStrict . Encoding.encodeUtf8

toDictionary :: ParsedDictionary
             -> Dictionary
toDictionary (ParsedDictionary termMap) =
  let
    toTerm (base, attrs) = Term base attrs
    toTermSet            = Set.fromList . map toTerm . Map.toList
  in
    Dictionary (toTermSet termMap)
