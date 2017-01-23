
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

-- | This function 'underscore' translate a field string from
-- CamelCase to its version using underscores.
underscore ∷ T.Text → T.Text
underscore field = T.pack $ camelTo2 '_' $ T.unpack field

-- | This function 'underscore' translate a field string from
-- CamelCase to its version using hypens.
hypen ∷ T.Text → T.Text
hypen field = T.pack $ camelTo2 '-' $T.unpack field

-- | This function 'lower' transforms the text to lower case.
lower ∷ T.Text → T.Text
lower = T.toLower

-- | This function 'upper' transforms the text to upper case.
upper ∷ T.Text → T.Text
upper = T.toUpper

-- | This function '.?.' checks if the JSON data has a field trying
-- with all variants of the string for the field, that includes using
-- CamelCase, underscores or hypens to replace white spaces, upper case
-- and lower case.
(.?.) :: FromJSON a => Object  → T.Text → Parser (Maybe a)
x .?. field = x .:? field
  <|> x .:? underscore field
  <|> x .:? hypen field
  <|> x .:? lower field
  <|> x .:? upper field

-- | This function '.:.' extracts from the JSON the field trying
-- with all variants of the string for the field, that includes using
-- CamelCase, underscores or hypens to replace white spaces, upper case
-- and lower case.
(.:.) :: FromJSON a => Object  → T.Text → Parser a
x .:. field = x .: field
  <|> x .: underscore field
  <|> x .: hypen field
  <|> x .: lower field
  <|> x .: upper field

-- | This function '.@.' parses a JSON data and extracts a specific field.
(.@.) ∷ FromJSON a ⇒ [Object] → T.Text → Parser a
[]  .@. _ = fail  "failed. Expected at least one key-value"
[x] .@. field = x .:. field
(x:xs) .@. field = do
  value ← x .?. field
  maybe (xs .@. field) return value

-- | Decode a Yaml structured file.
loadYAML ∷ FilePath → IO (Maybe Object)
loadYAML path = do
  decoded ← YamlInclude.decodeFileEither path
  case decoded of
    Right o   → return $ Just o
    Left msg  → do
      print msg
      return Nothing
