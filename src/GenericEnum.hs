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


gEnumToString :: (ConNames (Rep a), Generic a) => a -> String
gEnumToString x = conNameOf x

gEnumFromString :: forall a. (Generic a, Enum' (Rep a), ConNames (Rep a))
                => String -> Maybe a
gEnumFromString s = lookup s lookupTable
  where
    lookupTable :: [(String, a)]
    lookupTable = zip (conNameOf <$> (genumDefault :: [a]))  genumDefault
