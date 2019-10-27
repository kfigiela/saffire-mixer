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

import           Control.Lens           (at, non, (?~))
import           Control.Lens.TH        (makeFieldsNoPrefix, makeLenses)
import           Data.Aeson             (FromJSON, FromJSONKey, FromJSONKeyFunction (FromJSONKeyTextParser), ToJSON,
                                         ToJSONKey, fromJSONKey, genericParseJSON, genericToJSON, parseJSON, toJSON,
                                         toJSONKey)
import           Data.Aeson.Extra       (stripLensPrefix)
import           Data.Aeson.Types       (toJSONKeyText)
import           Data.Bits.Lens         (bitAt, byteAt)
import           Data.Default.Class     (Default, def)
import qualified Data.Map               as Map
import           Fmt                    ((+||), (||+))
import           GenericEnum            (gEnumFromString, gEnumToString)

import qualified SaffireLE.Mixer.HiRes  as H
import qualified SaffireLE.Mixer.Matrix as L
import           SaffireLE.RawControl   (RawControl (..), RawControlValue)
import           SaffireLE.Utils        (toBool)

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


data MixerState
    = MixerState
    { _lowResMixer      :: L.MatrixMixer
    , _highResMixer     :: H.Mixer
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
    f = maybe (const id) fst $ mixerControlsMap ^. at control
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
    , (Out12ToSpdifOut,   booleanControl (lowResMixer . L.out12ToSpdif))
    , (Pc1ToOut1,         mixControl (lowResMixer . L.out1 . L.dac1))
    , (Pc1ToOut3,         mixControl (lowResMixer . L.out3 . L.dac1))
    , (Pc1ToOut2,         mixControl (lowResMixer . L.out2 . L.dac1))
    , (Pc1ToOut4,         mixControl (lowResMixer . L.out4 . L.dac1))
    , (Pc3ToOut1,         mixControl (lowResMixer . L.out1 . L.dac3))
    , (Pc3ToOut3,         mixControl (lowResMixer . L.out3 . L.dac3))
    , (Pc3ToOut2,         mixControl (lowResMixer . L.out2 . L.dac3))
    , (Pc3ToOut4,         mixControl (lowResMixer . L.out4 . L.dac3))
    , (Pc5ToOut1,         mixControl (lowResMixer . L.out1 . L.dac5))
    , (Pc5ToOut3,         mixControl (lowResMixer . L.out3 . L.dac5))
    , (Pc5ToOut2,         mixControl (lowResMixer . L.out2 . L.dac5))
    , (Pc5ToOut4,         mixControl (lowResMixer . L.out4 . L.dac5))
    , (Pc7ToOut1,         mixControl (lowResMixer . L.out1 . L.dac7))
    , (Pc7ToOut3,         mixControl (lowResMixer . L.out3 . L.dac7))
    , (Pc7ToOut2,         mixControl (lowResMixer . L.out2 . L.dac7))
    , (Pc7ToOut4,         mixControl (lowResMixer . L.out4 . L.dac7))
    , (Pc2ToOut1,         mixControl (lowResMixer . L.out1 . L.dac2))
    , (Pc2ToOut3,         mixControl (lowResMixer . L.out3 . L.dac2))
    , (Pc2ToOut2,         mixControl (lowResMixer . L.out2 . L.dac2))
    , (Pc2ToOut4,         mixControl (lowResMixer . L.out4 . L.dac2))
    , (Pc4ToOut1,         mixControl (lowResMixer . L.out1 . L.dac4))
    , (Pc4ToOut3,         mixControl (lowResMixer . L.out3 . L.dac4))
    , (Pc4ToOut2,         mixControl (lowResMixer . L.out2 . L.dac4))
    , (Pc4ToOut4,         mixControl (lowResMixer . L.out4 . L.dac4))
    , (Pc6ToOut1,         mixControl (lowResMixer . L.out1 . L.dac6))
    , (Pc6ToOut3,         mixControl (lowResMixer . L.out3 . L.dac6))
    , (Pc6ToOut2,         mixControl (lowResMixer . L.out2 . L.dac6))
    , (Pc6ToOut4,         mixControl (lowResMixer . L.out4 . L.dac6))
    , (Pc8ToOut1,         mixControl (lowResMixer . L.out1 . L.dac8))
    , (Pc8ToOut3,         mixControl (lowResMixer . L.out3 . L.dac8))
    , (Pc8ToOut2,         mixControl (lowResMixer . L.out2 . L.dac8))
    , (Pc8ToOut4,         mixControl (lowResMixer . L.out4 . L.dac8))
    , (In1ToOut1,         mixControl (lowResMixer . L.out1 . L.in1))
    , (In1ToOut3,         mixControl (lowResMixer . L.out3 . L.in1))
    , (In1ToOut2,         mixControl (lowResMixer . L.out2 . L.in1))
    , (In1ToOut4,         mixControl (lowResMixer . L.out4 . L.in1))
    , (In3ToOut1,         mixControl (lowResMixer . L.out1 . L.in3))
    , (In3ToOut3,         mixControl (lowResMixer . L.out3 . L.in3))
    , (In3ToOut2,         mixControl (lowResMixer . L.out2 . L.in3))
    , (In3ToOut4,         mixControl (lowResMixer . L.out4 . L.in3))
    , (Spdif1ToOut1,      mixControl (lowResMixer . L.out1 . L.spdif1))
    , (Spdif1ToOut3,      mixControl (lowResMixer . L.out3 . L.spdif1))
    , (Spdif1ToOut2,      mixControl (lowResMixer . L.out2 . L.spdif1))
    , (Spdif1ToOut4,      mixControl (lowResMixer . L.out4 . L.spdif1))
    , (In2ToOut1,         mixControl (lowResMixer . L.out1 . L.in2))
    , (In2ToOut3,         mixControl (lowResMixer . L.out3 . L.in2))
    , (In2ToOut2,         mixControl (lowResMixer . L.out2 . L.in2))
    , (In2ToOut4,         mixControl (lowResMixer . L.out4 . L.in2))
    , (In4ToOut1,         mixControl (lowResMixer . L.out1 . L.in4))
    , (In4ToOut3,         mixControl (lowResMixer . L.out3 . L.in4))
    , (In4ToOut2,         mixControl (lowResMixer . L.out2 . L.in4))
    , (In4ToOut4,         mixControl (lowResMixer . L.out4 . L.in4))
    , (Spdif2ToOut1,      mixControl (lowResMixer . L.out1 . L.spdif2))
    , (Spdif2ToOut3,      mixControl (lowResMixer . L.out3 . L.spdif2))
    , (Spdif2ToOut2,      mixControl (lowResMixer . L.out2 . L.spdif2))
    , (Spdif2ToOut4,      mixControl (lowResMixer . L.out4 . L.spdif2))
    -- High res mixer
    , (Out12ToSpdifOut_96K,   booleanControl (highResMixer . H.out12ToSpdif))
    , (In1ToRecmix_96K,    mixControl (highResMixer . H.recMix . H.in1))
    , (In3ToRecmix_96K,    mixControl (highResMixer . H.recMix . H.in3))
    , (Spdif1ToRecmix_96K, mixControl (highResMixer . H.recMix . H.spdif1))
    , (In2ToRecmix_96K,    mixControl (highResMixer . H.recMix . H.in2))
    , (In4ToRecmix_96K,    mixControl (highResMixer . H.recMix . H.in4))
    , (Spdif2ToRecmix_96K, mixControl (highResMixer . H.recMix . H.spdif2))
    -- Out1
    , (RecmixToOut1_96K,  mixControl (highResMixer . H.out1 . H.recMix))
    , (Pc1ToOut1_96K,     mixControl (highResMixer . H.out1 . H.dac1))
    , (Pc2ToOut1_96K,     mixControl (highResMixer . H.out1 . H.dac3))
    -- Out3
    , (RecmixToOut3_96K,  mixControl (highResMixer . H.out2 . H.recMix))
    , (Pc1ToOut3_96K,     mixControl (highResMixer . H.out2 . H.dac2))
    , (Pc2ToOut3_96K,     mixControl (highResMixer . H.out2 . H.dac4))
    -- Out2
    , (RecmixToOut2_96K,  mixControl (highResMixer . H.out3 . H.recMix))
    , (Pc1ToOut2_96K,     mixControl (highResMixer . H.out3 . H.dac1))
    , (Pc2ToOut2_96K,     mixControl (highResMixer . H.out3 . H.dac3))
    -- Out 4
    , (RecmixToOut4_96K,  mixControl (highResMixer . H.out4 . H.recMix))
    , (Pc1ToOut4_96K,     mixControl (highResMixer . H.out4 . H.dac2))
    , (Pc2ToOut4_96K,     mixControl (highResMixer . H.out4 . H.dac4))
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
