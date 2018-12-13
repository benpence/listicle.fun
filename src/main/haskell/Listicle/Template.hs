{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Applicative             as Applicative
import qualified Text.Templating.Heist           as Heist

import Text.Templating.Heist (HeistState)

module Listicle.Template
( build
, Templates
, renderRandom
) where

data Templates
    = Templates
      { tempNames :: Set Text
      , tempState :: HeistState m
      }

init :: Templates
init = Templates (Heist.init)

add :: Templates
    -> Text
    -> Text
    -> Templates
add templates name content = undefined

renderRandom :: Templates
             => [Story]
             -> Text
renderRandom stories =
    Blaze.renderHtml $(shamletFile "mypage.hamlet")
