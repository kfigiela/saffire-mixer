{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.HiResStereo where

import           Universum

import           Control.Lens.TH        (makeFieldsNoPrefix)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Aeson.Extra       (StripLensPrefix (..))
import           Data.Default.Class     (Default, def)

import qualified SaffireLE.Mixer.HiRes  as H
import           SaffireLE.Mixer.Stereo (ChannelMix, channelMixToVolume, volumeToChannelMix)

type MixValue = Double

-- | 88.2 kHz and 96 kHz sample rate mixers
data StereoMixer
    = StereoMixer
    { _stereo1      :: StereoMix
    , _stereo2      :: StereoMix
    , _recMix       :: H.RecMix
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

makeFieldsNoPrefix ''StereoMixer
makeFieldsNoPrefix ''StereoMix

toStereoMixer :: H.Mixer -> StereoMixer
toStereoMixer mix =
    StereoMixer
    { _stereo1 = toStereoMix (mix ^. H.out12)
    , _stereo2 = toStereoMix (mix ^. H.out34)
    , _recMix = mix ^. H.recMix
    , _out12ToSpdif = mix ^. H.out12ToSpdif
    }

toStereoMix :: H.Mix -> StereoMix
toStereoMix out =
   StereoMix
    { _dac12  = volumeToChannelMix (out ^. H.dac12  . _1, out ^. H.dac12  . _2)
    , _dac34  = volumeToChannelMix (out ^. H.dac34  . _1, out ^. H.dac34  . _2)
    , _recMix = volumeToChannelMix (out ^. H.recMix . _1, out ^. H.recMix . _2)
    }

fromStereoMixer :: StereoMixer -> H.Mixer
fromStereoMixer mix =
    H.Mixer
    { H._out12 = fromStereoMix (mix ^. stereo1)
    , H._out34 = fromStereoMix (mix ^. stereo2)
    , H._recMix = mix ^. recMix
    , H._out12ToSpdif = mix ^. out12ToSpdif
    }

fromStereoMix :: StereoMix -> H.Mix
fromStereoMix mix =
    H.Mix
    { H._dac12  = channelMixToVolume 1.0 (mix ^. dac12)
    , H._dac34  = channelMixToVolume 1.0 (mix ^. dac34)
    , H._recMix = channelMixToVolume 1.0 (mix ^. recMix)
    }
