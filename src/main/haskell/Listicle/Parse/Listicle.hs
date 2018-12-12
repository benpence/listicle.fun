{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Parse.Listicle
( parse
) where

import qualified Data.Attoparsec.Text            as Parser
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Listicle.Util                   as Util

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)

import Listicle.Types

parse :: Text
      -> Either Text Listicle
parse = Bifunctor.first Text.pack . Parser.parseOnly listicle

listicle :: Parser Listicle
listicle = do
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
