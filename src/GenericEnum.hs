{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module GenericEnum where

import           Generics.Deriving
import           GHC.Generics
import           Prelude


gEnumToString :: (ConNames (Rep a), Generic a) => a -> String
gEnumToString x = conNameOf x

gEnumFromString :: forall a. (Generic a, Enum' (Rep a), ConNames (Rep a))
                => String -> Maybe a
gEnumFromString s = lookup s lookupTable
  where
    lookupTable :: [(String, a)]
    lookupTable = zip (conNameOf <$> (genumDefault :: [a]))  genumDefault
