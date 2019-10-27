{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.Stereo where

import           Universum

import           Control.Lens.TH        (makeFieldsNoPrefix)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Aeson.Extra       (StripLensPrefix (..))
import           Data.Default.Class     (Default, def)

import           SaffireLE.Mixer.Matrix (MatrixMixer (..), Mix (..))
import qualified SaffireLE.Mixer.Matrix as Mixer

type MixValue = Double

data StereoMixer
    = StereoMixer
    { _stereo1      :: StereoMix
    , _stereo2      :: StereoMix
    , _out12ToSpdif :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix StereoMixer)
instance Default  StereoMixer where def = StereoMixer def def False

data StereoMix
    = StereoMix
    { _stereoDAC12   :: StereoMixValue
    , _stereoDAC34   :: StereoMixValue
    , _stereoDAC56   :: StereoMixValue
    , _stereoDAC78   :: StereoMixValue
    , _stereoIn12    :: StereoMixValue
    , _stereoIn34    :: StereoMixValue
    , _stereoSpdif12 :: StereoMixValue
    , _inputMix      :: Double
    , _dacMix        :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix StereoMix)

instance Default StereoMix where def = StereoMix defStereoPair defStereoPair defStereoPair defStereoPair defMonoPair defMonoPair defStereoPair 1.0 1.0

-- | Represents mix of stereo channel
data ChannelMix
    = ChannelMix
    { _volume  :: MixValue
    , _balance :: MixValue
    , _enabled :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix ChannelMix)

instance Default ChannelMix where def = ChannelMix 1.0 0.0 True

data StereoMixValue
    = StereoPair
    { _ch12Volume :: ChannelMix
    }
    | MonoPair
    { _ch1Volume :: ChannelMix
    , _ch2Volume :: ChannelMix
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix StereoMixValue)


defStereoPair :: StereoMixValue
defStereoPair = StereoPair def

defMonoPair :: StereoMixValue
defMonoPair = MonoPair def def

makeFieldsNoPrefix ''StereoMixer
makeFieldsNoPrefix ''StereoMix
makeFieldsNoPrefix ''ChannelMix
makeFieldsNoPrefix ''StereoMixValue

toChannelMix :: StereoMixValue -> (ChannelMix, ChannelMix)
toChannelMix (StereoPair (ChannelMix volume balance enabled)) = (ChannelMix (l * volume) (-1.0) enabled , ChannelMix (r * volume) 1.0 enabled)
    where
    (l, r) = balanceToVolume balance
toChannelMix (MonoPair ch1 ch2)          = (ch1, ch2)

balanceToVolume :: Double -> (Double, Double)
balanceToVolume bal = (l, r) where
    l = max 0 $ min 1 (1 - bal)
    r = max 0 $ min 1 (1 + bal)

channelMixToVolume :: Double -> ChannelMix -> (Double, Double)
channelMixToVolume coeff (ChannelMix vol bal enabled) = (vol * l * coeff * enabled', vol * r * coeff * enabled')
    where
    (l, r) = balanceToVolume bal
    enabled' = if enabled then 1.0 else 0.0

volumeToChannelMix :: (Double, Double) -> ChannelMix
volumeToChannelMix (l, r) = ChannelMix vol bal True where
    vol = max l r
    bal = if vol == 0 then 0 else (r - l) / vol

channelPairToStereoMix :: (ChannelMix, ChannelMix) -> StereoMixValue
channelPairToStereoMix (ChannelMix v1 (-1.0) enabled1, ChannelMix v2 1.0 enabled2) | v1 == v2 && enabled1 == enabled2 = StereoPair (ChannelMix v1 0.0 enabled1)
channelPairToStereoMix (ch1, ch2) = MonoPair ch1 ch2

fromFullMix :: ((Double, Double), (Double, Double)) -> StereoMixValue
fromFullMix (l, r) = channelPairToStereoMix (volumeToChannelMix l, volumeToChannelMix r)

fullMix :: Double -> StereoMixValue -> ((Double, Double), (Double, Double))
fullMix coeff mix = (channelMixToVolume coeff ch1Mix, channelMixToVolume coeff ch2Mix) where (ch1Mix, ch2Mix) = toChannelMix mix

stereoMixToLowResMix :: StereoMix -> (Mix, Mix)
stereoMixToLowResMix mix
    = (l, r)
    where
    l = Mix
        { _dac1         = ldac1
        , _dac2         = ldac2
        , _dac3         = ldac3
        , _dac4         = ldac4
        , _dac5         = ldac5
        , _dac6         = ldac6
        , _dac7         = ldac7
        , _dac8         = ldac8
        , _in1          = lin1
        , _in2          = lin2
        , _in3          = lin3
        , _in4          = lin4
        , _spdif1       = lspdif1
        , _spdif2       = lspdif2
        }
    r = Mix
        { _dac1         = rdac1
        , _dac2         = rdac2
        , _dac3         = rdac3
        , _dac4         = rdac4
        , _dac5         = rdac5
        , _dac6         = rdac6
        , _dac7         = rdac7
        , _dac8         = rdac8
        , _in1          = rin1
        , _in2          = rin2
        , _in3          = rin3
        , _in4          = rin4
        , _spdif1       = rspdif1
        , _spdif2       = rspdif2
        }

    ((ldac1, rdac1), (ldac2, rdac2)) = fullMix (mix ^. dacMix) (mix ^. stereoDAC12)
    ((ldac3, rdac3), (ldac4, rdac4)) = fullMix (mix ^. dacMix) (mix ^. stereoDAC34)
    ((ldac5, rdac5), (ldac6, rdac6)) = fullMix (mix ^. dacMix) (mix ^. stereoDAC56)
    ((ldac7, rdac7), (ldac8, rdac8)) = fullMix (mix ^. dacMix) (mix ^. stereoDAC78)
    ((lin1,  rin1),  (lin2,  rin2))  = fullMix (mix ^. inputMix) (mix ^. stereoIn12)
    ((lin3,  rin3),  (lin4,  rin4))  = fullMix (mix ^. inputMix) (mix ^. stereoIn34)
    ((lspdif1, rspdif1),  (lspdif2,  rspdif2))  = fullMix (mix ^. inputMix) (mix ^. stereoSpdif12)


lowResMixToStereo :: (Mix, Mix) -> StereoMix
lowResMixToStereo (l, r) = mix
    where
    Mix
        { _dac1         = ldac1
        , _dac2         = ldac2
        , _dac3         = ldac3
        , _dac4         = ldac4
        , _dac5         = ldac5
        , _dac6         = ldac6
        , _dac7         = ldac7
        , _dac8         = ldac8
        , _in1          = lin1
        , _in2          = lin2
        , _in3          = lin3
        , _in4          = lin4
        , _spdif1       = lspdif1
        , _spdif2       = lspdif2
        } = l
    Mix
        { _dac1         = rdac1
        , _dac2         = rdac2
        , _dac3         = rdac3
        , _dac4         = rdac4
        , _dac5         = rdac5
        , _dac6         = rdac6
        , _dac7         = rdac7
        , _dac8         = rdac8
        , _in1          = rin1
        , _in2          = rin2
        , _in3          = rin3
        , _in4          = rin4
        , _spdif1       = rspdif1
        , _spdif2       = rspdif2
        } = r
    mix = StereoMix
        { _stereoDAC12 = fromFullMix ((ldac1, rdac1), (ldac2, rdac2))
        , _stereoDAC34 = fromFullMix ((ldac3, rdac3), (ldac4, rdac4))
        , _stereoDAC56 = fromFullMix ((ldac5, rdac5), (ldac6, rdac6))
        , _stereoDAC78 = fromFullMix ((ldac7, rdac7), (ldac8, rdac8))
        , _stereoIn12 = fromFullMix ((lin1,  rin1),  (lin2,  rin2))
        , _stereoIn34 = fromFullMix ((lin3,  rin3),  (lin4,  rin4))
        , _stereoSpdif12 = fromFullMix ((lspdif1, rspdif1),  (lspdif2,  rspdif2))
        , _inputMix = 1.0
        , _dacMix = 1.0
        }

toStereoMixer :: MatrixMixer -> StereoMixer
toStereoMixer mix =
    StereoMixer
    { _stereo1 = lowResMixToStereo (mix ^. Mixer.out1, mix ^. Mixer.out2)
    , _stereo2 = lowResMixToStereo (mix ^. Mixer.out3, mix ^. Mixer.out4)
    , _out12ToSpdif = mix ^. Mixer.out12ToSpdif
    }

fromSteroMixer :: StereoMixer -> MatrixMixer
fromSteroMixer mix =
    MatrixMixer
    { Mixer._out1 = out1
    , Mixer._out2 = out2
    , Mixer._out3 = out3
    , Mixer._out4 = out4
    , Mixer._out12ToSpdif = mix ^. out12ToSpdif
    }
    where
    (out1, out2) = stereoMixToLowResMix $ mix ^. stereo1
    (out3, out4) = stereoMixToLowResMix $ mix ^. stereo2
