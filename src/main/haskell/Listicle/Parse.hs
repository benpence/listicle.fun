{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Parse
( dictionary
, headline
) where

import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Types                as Aeson
import qualified Data.Attoparsec.Text            as Parser
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Encoding
import qualified Listicle.Util                   as Util
import qualified Text.Mustache                   as Mustache

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
    toTermArray          = Util.array . map toTerm . Map.toList
  in
    Dictionary (toTermArray termMap)

headline :: Text
         -> Either Text Headline
headline = Bifunctor.first Text.pack . Parser.parseOnly fullHeadline

fullHeadline :: Parser Headline
fullHeadline = do
    parts <- Parser.many1 headlinePart
    pure (Headline (map transformIfNumber parts))

transformIfNumber :: HeadlinePart
                  -> HeadlinePart
transformIfNumber lp
  | lp == numberFillin = Number
  | otherwise          = lp

numberFillin :: HeadlinePart
numberFillin = FillIn (Util.array [FillInPath "number" Nothing])

headlinePart :: Parser HeadlinePart
headlinePart = fillIn <|> normalText

normalText :: Parser HeadlinePart
normalText = fmap NormalText (Parser.takeWhile1 ('[' /=))

fillIn :: Parser HeadlinePart
fillIn =
  let
    sep = Parser.skipSpace <* Parser.char '/' <* Parser.skipSpace
  in do
    Parser.char '['
    fillInPaths <- Parser.sepBy fillInPath sep
    Parser.char ']'
    pure (FillIn (Util.array fillInPaths))

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
