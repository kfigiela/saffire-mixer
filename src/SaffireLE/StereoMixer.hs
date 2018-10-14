{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE DeriveAnyClass #-}

module SaffireLE.StereoMixer where

import           Universum

import           Control.Lens.TH      (makeFieldsNoPrefix, makeLenses)
import           Data.Aeson           (FromJSON, FromJSONKey, FromJSONKeyFunction (FromJSONKeyTextParser),
                                       ToJSON, ToJSONKey, fromJSONKey,
                                       genericParseJSON, genericToJSON,
                                       parseJSON, toJSON, toJSONKey)
import           Data.Aeson.Extra     (stripLensPrefix)
import           Data.Default.Class   (Default, def)

import SaffireLE.Mixer (LowResMix(..), LowResMixer(..))
import qualified SaffireLE.Mixer as Mixer

type MixValue = Double

data StereoMixer
    = StereoMixer
    { _stereo1      :: StereoMix
    , _stereo2      :: StereoMix
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)


instance ToJSON   StereoMixer where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON StereoMixer where parseJSON = genericParseJSON stripLensPrefix
instance Default  StereoMixer where def = StereoMixer def def False

data StereoMix
    = StereoMix
    { _stereoDAC12         :: StereoMixValue
    , _stereoDAC34         :: StereoMixValue
    , _stereoDAC56         :: StereoMixValue
    , _stereoDAC78         :: StereoMixValue
    , _stereoIn12          :: StereoMixValue
    , _stereoIn34          :: StereoMixValue
    , _stereoSpdif12       :: StereoMixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   StereoMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON StereoMix where parseJSON = genericParseJSON stripLensPrefix
instance Default StereoMix where def = StereoMix defStereoPair defStereoPair defStereoPair defStereoPair defMonoPair defMonoPair defStereoPair

data ChannelMix
    = ChannelMix
    { _volume :: MixValue
    , _balance :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   ChannelMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON ChannelMix where parseJSON = genericParseJSON stripLensPrefix


data StereoMixValue
    = StereoPair
    { _ch12Volume :: MixValue
    }
    | MonoPair
    { _ch1Volume :: ChannelMix
    , _ch2Volume :: ChannelMix
    } deriving (Show, Eq, Generic)


defStereoPair :: StereoMixValue
defStereoPair = StereoPair 1.0

defMonoPair :: StereoMixValue
defMonoPair = MonoPair (ChannelMix 1.0 0.0) (ChannelMix 1.0 0.0)


instance ToJSON   StereoMixValue where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON StereoMixValue where parseJSON = genericParseJSON stripLensPrefix


makeFieldsNoPrefix ''StereoMixer
makeFieldsNoPrefix ''StereoMix
makeFieldsNoPrefix ''ChannelMix
makeFieldsNoPrefix ''StereoMixValue

toChannelMix :: StereoMixValue -> (ChannelMix, ChannelMix)
toChannelMix (StereoPair volume) = (ChannelMix volume (-1.0), ChannelMix volume 1.0)
toChannelMix (MonoPair ch1 ch2) = (ch1, ch2)

balanceToVolume :: Double -> (Double, Double)
balanceToVolume bal = (l, r) where
    l = max 0 $ min 1 (1 - bal)
    r = max 0 $ min 1 (1 + bal)

channelMixToVolume :: ChannelMix -> (Double, Double)
channelMixToVolume (ChannelMix vol bal) = (vol * l, vol * r) where (l, r) = balanceToVolume bal

volumeToChannelMix :: (Double, Double) -> ChannelMix
volumeToChannelMix (l, r) = ChannelMix vol bal where
    vol = max l r
    bal = if vol == 0 then 0 else (r - l) / vol

channelPairToStereoMix :: (ChannelMix, ChannelMix) -> StereoMixValue
channelPairToStereoMix (ChannelMix v1 (-1.0), ChannelMix v2 (1.0)) | v1 == v2 = StereoPair v1
channelPairToStereoMix (ch1, ch2) = MonoPair ch1 ch2

fromFullMix :: ((Double, Double), (Double, Double)) -> StereoMixValue
fromFullMix (l, r) = channelPairToStereoMix (volumeToChannelMix l, volumeToChannelMix r)

fullMix :: StereoMixValue -> ((Double, Double), (Double, Double))
fullMix mix = (channelMixToVolume ch1Mix, channelMixToVolume ch2Mix) where (ch1Mix, ch2Mix) = toChannelMix mix

stereoMixToLowResMix :: StereoMix -> (LowResMix, LowResMix)
stereoMixToLowResMix mix
    = (l, r)
    where
    l = LowResMix
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
    r = LowResMix
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

    ((ldac1, rdac1), (ldac2, rdac2)) = fullMix (mix ^. stereoDAC12)
    ((ldac3, rdac3), (ldac4, rdac4)) = fullMix (mix ^. stereoDAC34)
    ((ldac5, rdac5), (ldac6, rdac6)) = fullMix (mix ^. stereoDAC56)
    ((ldac7, rdac7), (ldac8, rdac8)) = fullMix (mix ^. stereoDAC78)
    ((lin1,  rin1),  (lin2,  rin2))  = fullMix (mix ^. stereoIn12)
    ((lin3,  rin3),  (lin4,  rin4))  = fullMix (mix ^. stereoIn34)
    ((lspdif1, rspdif1),  (lspdif2,  rspdif2))  = fullMix (mix ^. stereoSpdif12)


lowResMixToStereo :: (LowResMix, LowResMix) -> StereoMix
lowResMixToStereo (l, r) = mix
    where
    LowResMix
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
    LowResMix
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
        }

toStereoMixer :: LowResMixer -> StereoMixer
toStereoMixer mix =
    StereoMixer
    { _stereo1 = lowResMixToStereo (mix ^. Mixer.out1, mix ^. Mixer.out2)
    , _stereo2 = lowResMixToStereo (mix ^. Mixer.out3, mix ^. Mixer.out4)
    , _out12ToSpdif = mix ^. Mixer.out12ToSpdif
    }






-- (out12DacMix, out12InMix)

-- out1 . dac1 .~ out12DacMix * (stereo ^. stereoDac1)
