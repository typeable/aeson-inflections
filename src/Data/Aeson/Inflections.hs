module Data.Aeson.Inflections
  ( defaultFieldLabelModifier
  , defaultFieldLabelModifier'
  , deriveDefaultJSON
  , deriveEnumJSON
  , dropPrefix
  , dropPrefixLower
  , dropByPrefixLength
  , cutPrefix
  , toUnderscore
  , toDashed
  , toSpaced
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T (length, pack, unpack)
import           Data.Void
import           Language.Haskell.TH
import qualified Text.Inflections as I
import           Text.Megaparsec.Error


-- | Drops until first uppercase character and apply 'toUnderscore' to
-- the rest
defaultFieldLabelModifier :: String -> String
defaultFieldLabelModifier = toUnderscore . dropPrefix

defaultFieldLabelModifier' :: Text -> Text
defaultFieldLabelModifier' = T.pack . defaultFieldLabelModifier . T.unpack

deriveDefaultJSON :: Name -> Q [Dec]
deriveDefaultJSON = deriveJSON defaultOptions
  { fieldLabelModifier = defaultFieldLabelModifier }

deriveEnumJSON :: String -> Name -> Q [Dec]
deriveEnumJSON prefix = deriveJSON defaultOptions
  { constructorTagModifier = toUnderscore . cutPrefix prefix }

-- | Just drops prefix
dropPrefix :: String -> String
dropPrefix = dropWhile (not . isUpper)

-- | The same as @Prelude.drop@, but takes a string, instead of a length.
-- This makes code more explicit (easier to see what exactly we are expecting to
-- @drop@)
dropByPrefixLength :: Text -> (String -> String)
dropByPrefixLength t = drop (T.length t)

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
