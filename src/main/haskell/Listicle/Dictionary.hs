{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Dictionary
( load
, parse
) where

import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as Aeson
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Encoding
import qualified Data.Traversable                as Traversable
import qualified Listicle.Util                   as Util
import qualified System.Directory                as Directory

import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Listicle.Types

load :: FilePath
     -> ExceptT Text IO (Map Text Dictionary)
load dictionaryDir = do
    files <- Trans.lift (Directory.getDirectoryContents dictionaryDir)
    let dictFiles = filter (List.isSuffixOf ".json") files

    dictionaryList <- Traversable.forM dictFiles $ \fileName -> do
        let name = Text.pack (List.take (length fileName - 5) fileName)

        content <- Trans.lift (readFile (dictionaryDir <> "/" <> fileName))
        dict    <- parse (Text.pack content)

        pure (name, dict)

    pure (Map.fromList dictionaryList)

newtype ParsedDictionary
    = ParsedDictionary (Map Text (Map Text Text))
    deriving (Eq, Show, Ord, Generic)

instance Aeson.FromJSON ParsedDictionary

parse :: (Monad m)
      => Text
      -> ExceptT Text m Dictionary
parse =
  let
    prepare = Bifunctor.bimap Text.pack toDictionary
    raise   = Except.ExceptT . pure
  in
    raise . prepare . Aeson.eitherDecode . ByteString.fromStrict . Encoding.encodeUtf8

toDictionary :: ParsedDictionary
             -> Dictionary
toDictionary (ParsedDictionary termMap) =
  let
    toTerm (base, attrs) = Term base attrs
    toTermArray          = Util.array . map toTerm . Map.toList
  in
    Dictionary (toTermArray termMap)
