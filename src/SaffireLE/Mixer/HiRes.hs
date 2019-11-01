{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.HiRes where

import           Universum

import           Control.Lens.TH    (makeFieldsNoPrefix)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Aeson.Extra   (StripLensPrefix (..))
import           Data.Default.Class (Default, def)

type MixValue = Double

-- | 88.2 kHz and 96 kHz sample rate mixers
data Mixer
    = Mixer
    { _out12        :: Mix
    , _out34        :: Mix
    , _recMix       :: RecMix
    , _out12ToSpdif :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix Mixer)
instance Default  Mixer where def = Mixer def def def False

data Mix
    = Mix
    { _dac12  :: (MixValue, MixValue)
    , _dac34  :: (MixValue, MixValue)
    , _recMix :: (MixValue, MixValue)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix Mix)

data RecMix
    = RecMix
    { _in1    :: MixValue
    , _in2    :: MixValue
    , _in3    :: MixValue
    , _in4    :: MixValue
    , _spdif1 :: MixValue
    , _spdif2 :: MixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix RecMix)


makeFieldsNoPrefix ''Mixer
makeFieldsNoPrefix ''Mix
makeFieldsNoPrefix ''RecMix
