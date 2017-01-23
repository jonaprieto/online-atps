
-- | Yaml.hs module process .online-atps files.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module OnlineATPs.Utils.Yaml
  ( module YAP
  , (.:.)
  , (.?.)
  , (.@.)
  , hypen
  , loadYAML
  , lower
  , underscore
  , upper
  ) where

import Control.Applicative ( (<|>) )

import Data.Aeson          as YAP ( withObject )
import Data.Aeson.Types    as YAP ( camelTo2 )

import qualified Data.Text as T

import Data.Yaml           as YAP
import Data.Yaml.Include   as YamlInclude

-- | TODO
underscore ∷ T.Text → T.Text
underscore field = T.pack $ camelTo2 '_' $ T.unpack field

-- | TODO 
hypen ∷ T.Text → T.Text
hypen field = T.pack $ camelTo2 '-' $T.unpack field

-- | TODO 
lower ∷ T.Text → T.Text
lower = T.toLower

-- | TODO 
upper ∷ T.Text → T.Text
upper = T.toUpper

-- | TODO 
(.?.) :: FromJSON a => Object  → T.Text → Parser (Maybe a)
x .?. field = x .:? field
  <|> x .:? underscore field
  <|> x .:? hypen field
  <|> x .:? lower field
  <|> x .:? upper field

-- | TODO 
(.:.) :: FromJSON a => Object  → T.Text → Parser a
x .:. field = x .: field
  <|> x .: underscore field
  <|> x .: hypen field
  <|> x .: lower field
  <|> x .: upper field

-- | TODO 
(.@.) ∷ FromJSON a ⇒ [Object] → T.Text → Parser a
[]  .@. _ = fail  "failed. Expected at least one key-value"
[x] .@. field = x .:. field
(x:xs) .@. field = do
  value ← x .?. field
  maybe (xs .@. field) return value

-- | TODO 
loadYAML ∷ FilePath → IO (Maybe Object)
loadYAML path = do
  decoded ← YamlInclude.decodeFileEither path
  case decoded of
    Right o   → return $ Just o
    Left msg  → do
      print msg
      return Nothing
