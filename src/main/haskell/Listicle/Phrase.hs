{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Listicle.Phrase
( generate
, load
, parse
) where

import qualified Control.Monad.Morph             as Morph
import qualified Control.Monad.Random            as Random
import qualified Control.Monad.Trans.Class       as Trans
import qualified Control.Monad.Trans.Except      as Except
import qualified Data.Functor.Identity           as Identity
import qualified Data.Attoparsec.Text            as Parser
import qualified Data.Bifunctor                  as Bifunctor
import qualified Data.Map                        as Map
import qualified Data.Text                       as Text
import qualified Data.Traversable                as Traversable
import qualified Listicle.Util                   as Util

import Control.Applicative ((<|>))
import Control.Monad.Random (RandT)
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array (Array)
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import System.Random (RandomGen)

import Listicle.Types

generate :: (RandomGen g)
         => Config
         -> Array Int Phrase
         -> RandT g (Except Text) Text
generate config@(Config { .. }) phrases = do
    (Phrase parts) <- Util.randomFromArray phrases
    fmap (Util.capitalizeFirst . Text.concat) (mapM (generatePart config) parts)

generatePart :: (RandomGen g)
             => Config
             -> PhrasePart
             -> RandT g (Except Text) Text
generatePart _               (NormalText t)   = pure t
generatePart (Config { .. }) Number           = generateNumber configParams
generatePart (Config { .. }) (FillIn fillIns) = do
    (FillInPath { .. }) <- Util.randomFromArray fillIns
    dictionary          <- Trans.lift (Util.toExcept ("No such dictionary: " <> fipBase) (Map.lookup fipBase configDicts))
    generateFillIn dictionary fipAttr

generateNumber :: (RandomGen g)
               => Params
               -> RandT g (Except Text) Text
generateNumber (Params { .. }) = do
    randomNumber <- Random.getRandomR (paramsMinNumber, paramsMaxNumber)
    pure (Text.pack (show randomNumber))

generateFillIn :: (RandomGen g)
               => Dictionary
               -> Maybe Text
               -> RandT g (Except Text) Text
generateFillIn (Dictionary terms) attr = do
    (Term { .. }) <- Util.randomFromArray terms

    let generatedAttr = (\attr -> Map.lookup attr termAttrs) =<< attr

    -- TODO: This ignores problems where the requested attribute is missing
    Trans.lift (Util.rescue termBase generatedAttr)

load :: FilePath
     -> ExceptT Text IO (Array Int Phrase)
load phrasesPath = do
    lines <- Trans.lift (fmap lines (readFile phrasesPath))
    
    let exceptPhrases = sequence (map (parse . Text.pack) lines)

    Morph.hoist (pure . Identity.runIdentity) (fmap Util.array exceptPhrases)

parse :: Text
      -> Except Text Phrase
parse = Except.except . Bifunctor.first Text.pack . Parser.parseOnly fullPhraseParser

fullPhraseParser :: Parser Phrase
fullPhraseParser = do
    parts <- Parser.many1 phrasePartParser
    pure (Phrase (map transformIfNumber parts))

transformIfNumber :: PhrasePart
                  -> PhrasePart
transformIfNumber lp
  | lp == numberFillin = Number
  | otherwise          = lp

numberFillin :: PhrasePart
numberFillin = FillIn (Util.array [FillInPath "number" Nothing])

phrasePartParser :: Parser PhrasePart
phrasePartParser = fillInParser <|> normalTextParser

normalTextParser :: Parser PhrasePart
normalTextParser = fmap NormalText (Parser.takeWhile1 ('[' /=))

fillInParser :: Parser PhrasePart
fillInParser =
  let
    sep = Parser.skipSpace <* Parser.char '/' <* Parser.skipSpace
  in do
    Parser.char '['
    fillInPaths <- Parser.sepBy fillInPathParser sep
    Parser.char ']'
    pure (FillIn (Util.array fillInPaths))

fillInPathParser :: Parser FillInPath
fillInPathParser =
  let
    withoutAttr = do
        base <- wordParser
        pure (FillInPath base Nothing)
    withAttr = do
        base <- wordParser
        Parser.char '.'
        attr <- wordParser
        pure (FillInPath base (Just attr))
  in
    withAttr <|> withoutAttr

wordParser :: Parser Text
wordParser = fmap Text.pack (Parser.many1 Parser.letter)
