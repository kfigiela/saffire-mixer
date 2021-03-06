{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.Raw where

import           Universum

import           Control.Lens               (at)
import           Control.Lens.TH            (makeLenses)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Aeson.Extra           (StripLensPrefix (..))
import           Data.Bits.Lens             (bitAt, byteAt)
import           Data.Default.Class         (Default, def)
import qualified Data.Map                   as Map

import qualified SaffireLE.Mixer.Raw.HiRes  as H
import qualified SaffireLE.Mixer.Raw.LowRes as L
import           SaffireLE.RawControl       (RawControl (..), RawControlValue)
import           SaffireLE.Utils            (toBool)

type MixValue = Double

data OutOpts
    = OutOpts
    { _mute        :: Bool
    , _attenuation :: Word8
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix OutOpts)

instance Default OutOpts where
    def = OutOpts False 0

fromOutOpts :: OutOpts -> Word32
fromOutOpts (OutOpts mute attenuation) =
    (0 :: Word32)
    &  byteAt 0 .~ attenuation'
    &  bitAt 25 .~ mute
    where
        attenuation' :: Word8
        attenuation' = min attenuation 0x7f

toOutOpts :: Word32 -> OutOpts
toOutOpts value = OutOpts
    { _mute        = value ^. bitAt 25
    , _attenuation = value ^. byteAt 0
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
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix MixerState)

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
    , (RecmixToOut1_96K,  mixControl (highResMixer . H.out12 . H.recMix . _1))
    , (Pc1ToOut1_96K,     mixControl (highResMixer . H.out12 . H.dac12  . _1))
    , (Pc2ToOut1_96K,     mixControl (highResMixer . H.out12 . H.dac34  . _1))
    -- Out3
    , (RecmixToOut3_96K,  mixControl (highResMixer . H.out12 . H.recMix . _2))
    , (Pc1ToOut3_96K,     mixControl (highResMixer . H.out12 . H.dac12  . _2))
    , (Pc2ToOut3_96K,     mixControl (highResMixer . H.out12 . H.dac34  . _2))
    -- Out2
    , (RecmixToOut2_96K,  mixControl (highResMixer . H.out34 . H.recMix . _1))
    , (Pc1ToOut2_96K,     mixControl (highResMixer . H.out34 . H.dac12  . _1))
    , (Pc2ToOut2_96K,     mixControl (highResMixer . H.out34 . H.dac34  . _1))
    -- Out 4
    , (RecmixToOut4_96K,  mixControl (highResMixer . H.out34 . H.recMix . _2))
    , (Pc1ToOut4_96K,     mixControl (highResMixer . H.out34 . H.dac12  . _2))
    , (Pc2ToOut4_96K,     mixControl (highResMixer . H.out34 . H.dac34  . _2))
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
