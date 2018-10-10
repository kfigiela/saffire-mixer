{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GenericEnum where

import Prelude

import Text.Read
import GHC.Generics
import Generics.Deriving
import Control.Lens
import Data.Default as DD

data Options = Options
  {
    optConstructorTagModifier :: String -> String
  } deriving (Generic)

instance DD.Default Options where
  def = Options
    {
      optConstructorTagModifier = id
    }

makeLensesWith abbreviatedFields ''Options

gEnumToString :: (ConNames (Rep a), Generic a) => Options -> a -> String
gEnumToString opt x = (opt ^. constructorTagModifier) $ conNameOf x

gEnumFromString :: forall a. (Generic a, Enum' (Rep a), ConNames (Rep a))
                => Options -> String -> Maybe a
gEnumFromString opt s = lookup s lookupTable
  where
    lookupTable :: [(String, a)]
    lookupTable = zip (conNames (undefined :: a))  genumDefault
