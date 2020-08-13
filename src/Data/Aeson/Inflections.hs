module Data.Aeson.Inflections
  ( defaultFieldLabelModifier
  , defaultFieldLabelModifier'
  , dropPrefix
  , dropPrefixLower
  , cutPrefix
  , toUnderscore
  , toDashed
  , toSpaced
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Text as T (Text, pack, unpack)
import           Data.Void
import qualified Text.Inflections as I
import           Text.Megaparsec.Error


-- | Drops until first uppercase character and apply 'toUnderscore' to
-- the rest
defaultFieldLabelModifier :: String -> String
defaultFieldLabelModifier = toUnderscore . dropPrefix

defaultFieldLabelModifier' :: Text -> Text
defaultFieldLabelModifier' = T.pack . defaultFieldLabelModifier . T.unpack

-- | Just drops prefix
dropPrefix :: String -> String
dropPrefix = dropWhile (not . isUpper)

-- | Cuts the prefix or throws error if prefix does not match. For TH
-- modifiers only
cutPrefix :: String -> String -> String
cutPrefix pref s = if pref == take plen s
  then drop plen s
  else error "Prefix does not match!"
  where
    plen = length pref

-- | Drops prefix and turns first character to lowercase
dropPrefixLower :: String -> String
dropPrefixLower = over _head toLower . dropPrefix

toUnderscore :: String -> String
toUnderscore = fromInflection I.toUnderscore

toDashed :: String -> String
toDashed = fromInflection I.toDashed

fromInflection
  :: (Text -> Either (ParseErrorBundle Text Void) Text)
  -> String
  -> String
fromInflection f = either error' T.unpack . f . T.pack
  where
    error' = error . show . I.InflectionParsingFailed

toSpaced :: String -> String
toSpaced = fromInflection (I.toHumanized False <=< I.toUnderscore)
