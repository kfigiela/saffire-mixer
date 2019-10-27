{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
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
    { _out1         :: LMix
    , _out2         :: RMix
    , _out3         :: LMix
    , _out4         :: RMix
    , _recMix       :: RecMix
    , _out12ToSpdif :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix Mixer)
instance Default  Mixer where def = Mixer def def def def def False

data LMix
    = LMix
    { _dac1   :: MixValue
    , _dac3   :: MixValue
    , _recMix :: MixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix LMix)

data RMix
    = RMix
    { _dac2   :: MixValue
    , _dac4   :: MixValue
    , _recMix :: MixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix RMix)

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
makeFieldsNoPrefix ''LMix
makeFieldsNoPrefix ''RMix
makeFieldsNoPrefix ''RecMix
