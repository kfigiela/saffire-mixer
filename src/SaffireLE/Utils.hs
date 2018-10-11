module SaffireLE.Utils where

import           Universum

toBool :: Word32 -> Bool
toBool 0 = False
toBool 1 = True
toBool 2 = True
toBool v = error $ "Invalid boolean: " <> show v
