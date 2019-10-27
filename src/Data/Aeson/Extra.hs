{-# LANGUAGE UndecidableInstances #-} -- for GToJSON constraint

module Data.Aeson.Extra where

import Universum

import           Data.Aeson       (FromJSON, Options, Result (..), ToJSON, Value (..), defaultOptions,
                                   fieldLabelModifier, genericParseJSON, genericToJSON, parseJSON,
                                   tagSingleConstructors, toJSON, unwrapUnaryRecords)
import           Data.Aeson.Types (Parser, GToJSON, GFromJSON, Zero)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import           GHC.Generics     (Generic, Rep)

parseEnum :: (Text -> Maybe a) -> Value -> Parser a
parseEnum parser (String     (parser -> Just val)) = pure val
parseEnum parser (String str@(parser -> Nothing )) = fail $ show str <> " doesn't match any known enum values"
parseEnum _      v                                 = fail $ "Unsupported enum type: " <> show v

stripLensPrefix :: Options
stripLensPrefix = defaultOptions { fieldLabelModifier = drop 1 }

unwrapNewtypes :: Options
unwrapNewtypes = defaultOptions { unwrapUnaryRecords = True }

tagSingle :: Options
tagSingle = defaultOptions { tagSingleConstructors = True }

stripLensPrefixTagSingle :: Options
stripLensPrefixTagSingle = defaultOptions { fieldLabelModifier = drop 1, tagSingleConstructors = True }

tagErrors :: Options
tagErrors = tagSingle

resultToMaybe :: Result a -> Maybe a
resultToMaybe x = case x of
  Success a -> Just a
  _         -> Nothing

-- | A newtype wrapper which defines `FromJSON` and `ToJSON` instances generically derived with `stripLensPrefix` options.
--
-- Usage example:
--
-- Instead of:
--
-- @
--   data Foo = ...
--
--   instance ToJSON   Foo where    toJSON = genericToJSON    stripLensPrefix
--   instance FromJSON Foo where parseJSON = genericParseJSON stripLensPrefix
-- @
--
-- you can write:
--
-- @
--   data Foo = ... deriving (FromJSON, ToJSON) via (StripLensPrefix Foo)
-- @
newtype StripLensPrefix a = StripLensPrefix a

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (StripLensPrefix a) where
  toJSON (StripLensPrefix x) = genericToJSON stripLensPrefix x

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (StripLensPrefix a) where
  parseJSON = fmap StripLensPrefix . genericParseJSON stripLensPrefix


newtype StripLensPrefixTagSingle a = StripLensPrefixTagSingle a

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (StripLensPrefixTagSingle a) where
  toJSON (StripLensPrefixTagSingle x) = genericToJSON stripLensPrefixTagSingle x

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (StripLensPrefixTagSingle a) where
  parseJSON = fmap StripLensPrefixTagSingle . genericParseJSON stripLensPrefixTagSingle
