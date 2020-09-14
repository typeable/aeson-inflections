{-# LANGUAGE KindSignatures #-}

module Data.Aeson.Inflections.Generic
  -- Reexports from Deriving.Aeson
  ( CustomJSON(..)
  , FieldLabelModifier
  , ConstructorTagModifier
  , CamelToSnake
  , StripPrefix
  , OmitNothingFields

  , DropPrefix
  , DropPrefixLower
  , ToLower
  , DefaultFieldLabelModifier
  , DefaultJSON
  , DefaultEnumJSON
  )
  where

import Data.Aeson.Inflections
import Data.Char
import Deriving.Aeson
import GHC.TypeLits

data DropPrefix

instance StringModifier DropPrefix where
  getStringModifier = dropPrefix

data DropPrefixLower

instance StringModifier DropPrefixLower where
  getStringModifier = dropPrefixLower

data ToLower

instance StringModifier ToLower where
  getStringModifier = map toLower

type DefaultFieldLabelModifier = (DropPrefix, CamelToSnake)

type DefaultJSON = CustomJSON '[FieldLabelModifier DefaultFieldLabelModifier]

type DefaultEnumJSON (prefix :: Symbol)
  = CustomJSON '[ConstructorTagModifier (StripPrefix prefix, CamelToSnake)]
