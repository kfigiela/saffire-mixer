{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer where

import           Universum

import           Control.Lens         (at, non, (?~))
import           Control.Lens.TH      (makeFieldsNoPrefix, makeLenses)
import           Data.Aeson           (FromJSON, FromJSONKey, FromJSONKeyFunction (FromJSONKeyTextParser),
                                       ToJSON, ToJSONKey, fromJSONKey,
                                       genericParseJSON, genericToJSON,
                                       parseJSON, toJSON, toJSONKey)
import           Data.Aeson.Extra     (stripLensPrefix)
import           Data.Aeson.Types     (toJSONKeyText)
import           Data.Bits.Lens       (bitAt, byteAt)
import           Data.Default.Class   (Default, def)
import qualified Data.Map             as Map
import           Fmt                  ((+||), (||+))
import           GenericEnum          (gEnumFromString, gEnumToString)

import           SaffireLE.RawControl (RawControl (..), RawControlValue)
import           SaffireLE.Utils      (toBool)

type MixValue = Double

data OutOpts
    = OutOpts
    { _mute        :: Bool
    , _attenuation :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   OutOpts where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON OutOpts where parseJSON = genericParseJSON stripLensPrefix

instance Default OutOpts where
    def = OutOpts False 0

fromOutOpts :: OutOpts -> Word32
fromOutOpts (OutOpts mute attenuation) =
    (0 :: Word32)
    &  byteAt 0 .~ attenuation'
    &  bitAt 25 .~ mute
    where
        attenuation' :: Word8
        attenuation' = floor $ attenuation * 0x7f
toOutOpts :: Word32 -> OutOpts
toOutOpts value = OutOpts
    { _mute        = value ^. bitAt 25
    , _attenuation = (fromIntegral $ value ^. byteAt 0) / 0x7f
    }


-- | 44.1 kHz and 48 kHz sample rate mixers
data LowResMixer
    = LowResMixer
    { _out1         :: LowResMix
    , _out2         :: LowResMix
    , _out3         :: LowResMix
    , _out4         :: LowResMix
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   LowResMixer where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON LowResMixer where parseJSON = genericParseJSON stripLensPrefix
instance Default  LowResMixer where def = LowResMixer def def def def False

data LowResMix
    = LowResMix
    { _dac1         :: MixValue
    , _dac2         :: MixValue
    , _dac3         :: MixValue
    , _dac4         :: MixValue
    , _dac5         :: MixValue
    , _dac6         :: MixValue
    , _dac7         :: MixValue
    , _dac8         :: MixValue
    , _in1          :: MixValue
    , _in2          :: MixValue
    , _in3          :: MixValue
    , _in4          :: MixValue
    , _spdif1       :: MixValue
    , _spdif2       :: MixValue
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   LowResMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON LowResMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  LowResMix where def = LowResMix def def def def def def def def def def def def def def False


-- | 88.2 kHz and 96 kHz sample rate mixers
data HiResMixer
    = HiResMixer
    { _out1         :: HiResLMix
    , _out2         :: HiResRMix
    , _out3         :: HiResLMix
    , _out4         :: HiResRMix
    , _recMix       :: RecMix
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   HiResMixer where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON HiResMixer where parseJSON = genericParseJSON stripLensPrefix
instance Default  HiResMixer where def = HiResMixer def def def def def False

data HiResLMix
    = HiResLMix
    { _dac1   :: MixValue
    , _dac3   :: MixValue
    , _recMix :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   HiResLMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON HiResLMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  HiResLMix where def = HiResLMix def def def

data HiResRMix
    = HiResRMix
    { _dac2   :: MixValue
    , _dac4   :: MixValue
    , _recMix :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   HiResRMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON HiResRMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  HiResRMix where def = HiResRMix def def def

data RecMix
    = RecMix
    { _in1    :: MixValue
    , _in2    :: MixValue
    , _in3    :: MixValue
    , _in4    :: MixValue
    , _spdif1 :: MixValue
    , _spdif2 :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   RecMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON RecMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  RecMix where def = RecMix def def def def def def


makeFieldsNoPrefix ''LowResMixer
makeFieldsNoPrefix ''LowResMix
makeFieldsNoPrefix ''HiResMixer
makeFieldsNoPrefix ''HiResLMix
makeFieldsNoPrefix ''HiResRMix
makeFieldsNoPrefix ''RecMix

data MixerState
    = MixerState
    { _lowResMixer      :: LowResMixer
    , _highResMixer     :: HiResMixer
    , _in3Gain          :: Bool
    , _in4Gain          :: Bool
    , _out12Opts        :: OutOpts
    , _out34Opts        :: OutOpts
    , _out56Opts        :: OutOpts
    , _midiThru         :: Bool
    , _spdifTransparent :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   MixerState where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON MixerState where parseJSON = genericParseJSON stripLensPrefix

instance Default MixerState where
    def = MixerState
        { _lowResMixer = def
        , _highResMixer = def
        , _in3Gain = False
        , _in4Gain = False
        , _out12Opts = def
        , _out34Opts = def
        , _out56Opts = def
        , _midiThru = False
        , _spdifTransparent = False
        }

makeLenses ''MixerState
makeLenses ''OutOpts

updateMixerState :: MixerState -> [(RawControl, RawControlValue)] -> MixerState
updateMixerState = foldl updateMixerState'

updateMixerState' :: MixerState -> (RawControl, RawControlValue) -> MixerState
updateMixerState' state (control, value) = f value state where
    f = maybe (const identity) fst $ mixerControlsMap ^. at control
    mixerControlsMap = Map.fromList mixerControls

hardwarizeMixerState :: MixerState -> [(RawControl, RawControlValue)]
hardwarizeMixerState state = second (\(_r, w) -> w state) <$> mixerControls

mixerControls :: [(RawControl, (RawControlValue -> MixerState -> MixerState, MixerState -> RawControlValue))]
mixerControls =
    [ (HighGainLine3,     booleanControl in3Gain)
    , (HighGainLine4,     booleanControl in4Gain)
    , (BitfieldOut12,     optsControl out12Opts)
    , (BitfieldOut34,     optsControl out34Opts)
    , (BitfieldOut56,     optsControl out56Opts)
    , (Midithru,          booleanControl midiThru)
    , (SpdifTransparent,  booleanControl spdifTransparent)
    , (Out12ToSpdifOut,   booleanControl (lowResMixer . out12ToSpdif))
    , (Pc1ToOut1,         mixControl (lowResMixer . out1 . dac1))
    , (Pc1ToOut3,         mixControl (lowResMixer . out3 . dac1))
    , (Pc1ToOut2,         mixControl (lowResMixer . out2 . dac1))
    , (Pc1ToOut4,         mixControl (lowResMixer . out4 . dac1))
    , (Pc3ToOut1,         mixControl (lowResMixer . out1 . dac3))
    , (Pc3ToOut3,         mixControl (lowResMixer . out3 . dac3))
    , (Pc3ToOut2,         mixControl (lowResMixer . out2 . dac3))
    , (Pc3ToOut4,         mixControl (lowResMixer . out4 . dac3))
    , (Pc5ToOut1,         mixControl (lowResMixer . out1 . dac5))
    , (Pc5ToOut3,         mixControl (lowResMixer . out3 . dac5))
    , (Pc5ToOut2,         mixControl (lowResMixer . out2 . dac5))
    , (Pc5ToOut4,         mixControl (lowResMixer . out4 . dac5))
    , (Pc7ToOut1,         mixControl (lowResMixer . out1 . dac7))
    , (Pc7ToOut3,         mixControl (lowResMixer . out3 . dac7))
    , (Pc7ToOut2,         mixControl (lowResMixer . out2 . dac7))
    , (Pc7ToOut4,         mixControl (lowResMixer . out4 . dac7))
    , (Pc2ToOut1,         mixControl (lowResMixer . out1 . dac2))
    , (Pc2ToOut3,         mixControl (lowResMixer . out3 . dac2))
    , (Pc2ToOut2,         mixControl (lowResMixer . out2 . dac2))
    , (Pc2ToOut4,         mixControl (lowResMixer . out4 . dac2))
    , (Pc4ToOut1,         mixControl (lowResMixer . out1 . dac4))
    , (Pc4ToOut3,         mixControl (lowResMixer . out3 . dac4))
    , (Pc4ToOut2,         mixControl (lowResMixer . out2 . dac4))
    , (Pc4ToOut4,         mixControl (lowResMixer . out4 . dac4))
    , (Pc6ToOut1,         mixControl (lowResMixer . out1 . dac6))
    , (Pc6ToOut3,         mixControl (lowResMixer . out3 . dac6))
    , (Pc6ToOut2,         mixControl (lowResMixer . out2 . dac6))
    , (Pc6ToOut4,         mixControl (lowResMixer . out4 . dac6))
    , (Pc8ToOut1,         mixControl (lowResMixer . out1 . dac8))
    , (Pc8ToOut3,         mixControl (lowResMixer . out3 . dac8))
    , (Pc8ToOut2,         mixControl (lowResMixer . out2 . dac8))
    , (Pc8ToOut4,         mixControl (lowResMixer . out4 . dac8))
    , (In1ToOut1,         mixControl (lowResMixer . out1 . in1))
    , (In1ToOut3,         mixControl (lowResMixer . out3 . in1))
    , (In1ToOut2,         mixControl (lowResMixer . out2 . in1))
    , (In1ToOut4,         mixControl (lowResMixer . out4 . in1))
    , (In3ToOut1,         mixControl (lowResMixer . out1 . in3))
    , (In3ToOut3,         mixControl (lowResMixer . out3 . in3))
    , (In3ToOut2,         mixControl (lowResMixer . out2 . in3))
    , (In3ToOut4,         mixControl (lowResMixer . out4 . in3))
    , (Spdif1ToOut1,      mixControl (lowResMixer . out1 . spdif1))
    , (Spdif1ToOut3,      mixControl (lowResMixer . out3 . spdif1))
    , (Spdif1ToOut2,      mixControl (lowResMixer . out2 . spdif1))
    , (Spdif1ToOut4,      mixControl (lowResMixer . out4 . spdif1))
    , (In2ToOut1,         mixControl (lowResMixer . out1 . in2))
    , (In2ToOut3,         mixControl (lowResMixer . out3 . in2))
    , (In2ToOut2,         mixControl (lowResMixer . out2 . in2))
    , (In2ToOut4,         mixControl (lowResMixer . out4 . in2))
    , (In4ToOut1,         mixControl (lowResMixer . out1 . in4))
    , (In4ToOut3,         mixControl (lowResMixer . out3 . in4))
    , (In4ToOut2,         mixControl (lowResMixer . out2 . in4))
    , (In4ToOut4,         mixControl (lowResMixer . out4 . in4))
    , (Spdif2ToOut1,      mixControl (lowResMixer . out1 . spdif2))
    , (Spdif2ToOut3,      mixControl (lowResMixer . out3 . spdif2))
    , (Spdif2ToOut2,      mixControl (lowResMixer . out2 . spdif2))
    , (Spdif2ToOut4,      mixControl (lowResMixer . out4 . spdif2))
    -- High res mixer
    , (Out12ToSpdifOut_96K,   booleanControl (highResMixer . out12ToSpdif))
    , (In1ToRecmix_96K,    mixControl (highResMixer . recMix . in1))
    , (In3ToRecmix_96K,    mixControl (highResMixer . recMix . in3))
    , (Spdif1ToRecmix_96K, mixControl (highResMixer . recMix . spdif1))
    , (In2ToRecmix_96K,    mixControl (highResMixer . recMix . in2))
    , (In4ToRecmix_96K,    mixControl (highResMixer . recMix . in4))
    , (Spdif2ToRecmix_96K, mixControl (highResMixer . recMix . spdif2))
    -- Out1
    , (RecmixToOut1_96K,  mixControl (highResMixer . out1 . recMix))
    , (Pc1ToOut1_96K,     mixControl (highResMixer . out1 . dac1))
    , (Pc2ToOut1_96K,     mixControl (highResMixer . out1 . dac3))
    -- Out3
    , (RecmixToOut3_96K,  mixControl (highResMixer . out2 . recMix))
    , (Pc1ToOut3_96K,     mixControl (highResMixer . out2 . dac2))
    , (Pc2ToOut3_96K,     mixControl (highResMixer . out2 . dac4))
    -- Out2
    , (RecmixToOut2_96K,  mixControl (highResMixer . out3 . recMix))
    , (Pc1ToOut2_96K,     mixControl (highResMixer . out3 . dac1))
    , (Pc2ToOut2_96K,     mixControl (highResMixer . out3 . dac3))
    -- Out 4
    , (RecmixToOut4_96K,  mixControl (highResMixer . out4 . recMix))
    , (Pc1ToOut4_96K,     mixControl (highResMixer . out4 . dac2))
    , (Pc2ToOut4_96K,     mixControl (highResMixer . out4 . dac4))
    ]
  where
    booleanControl :: Lens' MixerState Bool -> (RawControlValue -> MixerState -> MixerState, MixerState -> RawControlValue)
    booleanControl l = (booleanControlR, booleanControlW) where
        booleanControlR value = l .~ toBool value
        booleanControlW state = if state ^. l then 1 else 0
    optsControl :: Lens' MixerState OutOpts -> (RawControlValue -> MixerState -> MixerState, MixerState -> RawControlValue)
    optsControl l = (optsControlR, optsControlW) where
        optsControlR value = l .~ toOutOpts value
        optsControlW state = fromOutOpts $ state ^. l
    mixControl :: Lens' MixerState MixValue -> (RawControlValue -> MixerState -> MixerState, MixerState -> RawControlValue)
    mixControl focus = (mixControlR, mixControlW) where
        mixControlR value = focus .~ (fromIntegral value / 0x7fff)
        mixControlW state = floor $ (state ^. focus) * 0x7fff

allControls :: [RawControl]
allControls = fst <$> mixerControls
