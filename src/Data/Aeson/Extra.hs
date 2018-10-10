
module Data.Aeson.Extra where

import Universum

import           Data.Aeson       (Options, Value (..), defaultOptions, fieldLabelModifier, tagSingleConstructors,
                                   unwrapUnaryRecords)
import           Data.Aeson.Types (Parser)

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
