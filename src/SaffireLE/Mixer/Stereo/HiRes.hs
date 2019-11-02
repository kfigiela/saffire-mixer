{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.Stereo.HiRes where

import           Universum

import           Control.Lens.TH               (makeFieldsNoPrefix)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Aeson.Extra              (StripLensPrefix (..))
import           Data.Default.Class            (Default, def)

import qualified SaffireLE.Mixer.Raw.HiRes     as H
import           SaffireLE.Mixer.Stereo.LowRes (ChannelMix, channelMixToVolume, volumeToChannelMix)

type MixValue = Double

-- | 88.2 kHz and 96 kHz sample rate mixers
data StereoMixer
    = StereoMixer
    { _stereo1      :: StereoMix
    , _stereo2      :: StereoMix
    , _recMix       :: RecMix
    , _out12ToSpdif :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix StereoMixer)
instance Default StereoMixer where def = StereoMixer def def def False

data StereoMix
    = StereoMix
    { _dac12  :: ChannelMix
    , _dac34  :: ChannelMix
    , _recMix :: ChannelMix
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix StereoMix)

data RecMixValue =
    RecMixValue
    { _enabled :: Bool
    , _volume  :: MixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix RecMixValue)

instance Default RecMixValue where def = RecMixValue False def

data RecMix =
    RecMix
    { _in1    :: RecMixValue
    , _in2    :: RecMixValue
    , _in3    :: RecMixValue
    , _in4    :: RecMixValue
    , _spdif1 :: RecMixValue
    , _spdif2 :: RecMixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix RecMix)


makeFieldsNoPrefix ''StereoMixer
makeFieldsNoPrefix ''StereoMix
makeFieldsNoPrefix ''RecMix

toStereoMixer :: H.Mixer -> StereoMixer
toStereoMixer mix =
    StereoMixer
    { _stereo1 = toStereoMix (mix ^. H.out12)
    , _stereo2 = toStereoMix (mix ^. H.out34)
    , _recMix = toRecMix (mix ^. H.recMix)
    , _out12ToSpdif = mix ^. H.out12ToSpdif
    }

toStereoMix :: H.Mix -> StereoMix
toStereoMix out =
   StereoMix
    { _dac12  = volumeToChannelMix (out ^. H.dac12  . _1, out ^. H.dac12  . _2)
    , _dac34  = volumeToChannelMix (out ^. H.dac34  . _1, out ^. H.dac34  . _2)
    , _recMix = volumeToChannelMix (out ^. H.recMix . _1, out ^. H.recMix . _2)
    }

toRecMix :: H.RecMix -> RecMix
toRecMix raw =
    RecMix
    { _in1    = RecMixValue { _enabled = True, _volume = raw ^. H.in1    }
    , _in2    = RecMixValue { _enabled = True, _volume = raw ^. H.in2    }
    , _in3    = RecMixValue { _enabled = True, _volume = raw ^. H.in3    }
    , _in4    = RecMixValue { _enabled = True, _volume = raw ^. H.in4    }
    , _spdif1 = RecMixValue { _enabled = True, _volume = raw ^. H.spdif1 }
    , _spdif2 = RecMixValue { _enabled = True, _volume = raw ^. H.spdif2 }
    }

fromStereoMixer :: StereoMixer -> H.Mixer
fromStereoMixer mix =
    H.Mixer
    { H._out12 = fromStereoMix (mix ^. stereo1)
    , H._out34 = fromStereoMix (mix ^. stereo2)
    , H._recMix = fromRecMix (mix ^. recMix)
    , H._out12ToSpdif = mix ^. out12ToSpdif
    }

fromStereoMix :: StereoMix -> H.Mix
fromStereoMix mix =
    H.Mix
    { H._dac12  = channelMixToVolume 1.0 (mix ^. dac12)
    , H._dac34  = channelMixToVolume 1.0 (mix ^. dac34)
    , H._recMix = channelMixToVolume 1.0 (mix ^. recMix)
    }

fromRecMix :: RecMix -> H.RecMix
fromRecMix mix =
    H.RecMix
    { H._in1    = fromRecMixValue $ mix ^. in1
    , H._in2    = fromRecMixValue $ mix ^. in2
    , H._in3    = fromRecMixValue $ mix ^. in3
    , H._in4    = fromRecMixValue $ mix ^. in4
    , H._spdif1 = fromRecMixValue $ mix ^. spdif1
    , H._spdif2 = fromRecMixValue $ mix ^. spdif2
    }

fromRecMixValue :: RecMixValue -> MixValue
fromRecMixValue RecMixValue { _enabled = False }                  = 0.0
fromRecMixValue RecMixValue { _enabled = True, _volume = volume } = volume
