{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Parse
( dictionary
, listicle
) where

import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as Aeson
import qualified Data.Attoparsec.Text            as Parser
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Encoding
import qualified Listicle.Util                   as Util

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Listicle.Types

newtype ParsedDictionary
    = ParsedDictionary (Map Text (Map Text Text))
    deriving (Eq, Show, Ord, Generic)

instance Aeson.FromJSON ParsedDictionary

dictionary :: Text -> Either Text Dictionary
dictionary =
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

listicle :: Text
         -> Either Text Listicle
listicle = Bifunctor.first Text.pack . Parser.parseOnly fullListicle

fullListicle :: Parser Listicle
fullListicle = do
    parts <- Parser.many1 listiclePart
    pure (Listicle (map transformIfNumber parts))

transformIfNumber :: ListiclePart
                  -> ListiclePart
transformIfNumber lp
  | lp == numberFillin = Number
  | otherwise          = lp

numberFillin :: ListiclePart
numberFillin = FillIn (Set.singleton (FillInPath "number" Nothing))

listiclePart :: Parser ListiclePart
listiclePart = fillIn <|> normalText

normalText :: Parser ListiclePart
normalText = fmap NormalText (Parser.takeWhile1 ('[' /=))

fillIn :: Parser ListiclePart
fillIn =
  let
    sep = Parser.skipSpace <* Parser.char '/' <* Parser.skipSpace
  in do
    Parser.char '['
    fillInPaths <- Parser.sepBy fillInPath sep
    Parser.char ']'
    pure (FillIn (Set.fromList fillInPaths))

fillInPath :: Parser FillInPath
fillInPath =
  let
    withoutAttr = do
        base <- word
        pure (FillInPath base Nothing)
    withAttr = do
        base <- word
        Parser.char '.'
        attr <- word
        pure (FillInPath base (Just attr))
  in
    withAttr <|> withoutAttr

word :: Parser Text
word = fmap Text.pack (Parser.many1 Parser.letter)