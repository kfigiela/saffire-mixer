{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module SaffireLE.Mixer where

import           Universum

import           Control.Lens         (at, non, (?~))
import           Control.Lens.TH      (makeLenses)
import           Data.Aeson           (FromJSON, FromJSONKey, FromJSONKeyFunction (FromJSONKeyTextParser),
                                       ToJSON, ToJSONKey, fromJSONKey,
                                       genericParseJSON, genericToJSON,
                                       parseJSON, toJSON, toJSONKey)
import           Data.Aeson.Extra     (stripLensPrefix)
import           Data.Aeson.Types     (toJSONKeyText)
import           Data.Bits.Lens       (bitAt, byteAt)
import           Data.Default         (Default, def)
import qualified Data.Map             as Map
import           Fmt                  ((+||), (||+))
import           GenericEnum          (gEnumFromString, gEnumToString)

import           SaffireLE.RawControl (RawControl (..), RawControlValue)
import           SaffireLE.Utils      (toBool)

data InChannel
    = In1
    | In2
    | In3
    | In4
    | SpdifIn1
    | SpdifIn2
    | DAC1
    | DAC2
    | DAC3
    | DAC4
    | DAC5
    | DAC6
    | DAC7
    | DAC8
    deriving (Show, Eq, Generic, Ord, FromJSON, ToJSON)

instance ToJSONKey InChannel where toJSONKey = toJSONKeyText $ toText . gEnumToString
instance FromJSONKey InChannel where fromJSONKey = FromJSONKeyTextParser $ \text -> maybe (fail "Invalid InChannel") pure (gEnumFromString $ toString text)

data OutChannel
    = Out1
    | Out2
    | Out3
    | Out4
    | Out5
    | Out6
    | SpdifOut7
    | SpdifOut8
    deriving (Show, Eq, Generic, Ord, FromJSON, ToJSON)

instance ToJSONKey OutChannel where toJSONKey = toJSONKeyText $ toText . gEnumToString
instance FromJSONKey OutChannel where fromJSONKey = FromJSONKeyTextParser $ \text -> maybe (fail "Invalid OutChannel") pure (gEnumFromString $ toString text)

data Mix = Mix { _in :: InChannel, _out :: OutChannel } deriving (Show, Eq, Generic, Ord, ToJSON, FromJSON)

type MixValue = Double

mixToControl :: Mix -> RawControl
mixToControl = \case
    Mix DAC1     Out1 -> Pc1ToOut1
    Mix DAC1     Out3 -> Pc1ToOut3
    Mix DAC1     Out2 -> Pc1ToOut2
    Mix DAC1     Out4 -> Pc1ToOut4
    Mix DAC3     Out1 -> Pc3ToOut1
    Mix DAC3     Out3 -> Pc3ToOut3
    Mix DAC3     Out2 -> Pc3ToOut2
    Mix DAC3     Out4 -> Pc3ToOut4
    Mix DAC5     Out1 -> Pc5ToOut1
    Mix DAC5     Out3 -> Pc5ToOut3
    Mix DAC5     Out2 -> Pc5ToOut2
    Mix DAC5     Out4 -> Pc5ToOut4
    Mix DAC7     Out1 -> Pc7ToOut1
    Mix DAC7     Out3 -> Pc7ToOut3
    Mix DAC7     Out2 -> Pc7ToOut2
    Mix DAC7     Out4 -> Pc7ToOut4
    Mix DAC2     Out1 -> Pc2ToOut1
    Mix DAC2     Out3 -> Pc2ToOut3
    Mix DAC2     Out2 -> Pc2ToOut2
    Mix DAC2     Out4 -> Pc2ToOut4
    Mix DAC4     Out1 -> Pc4ToOut1
    Mix DAC4     Out3 -> Pc4ToOut3
    Mix DAC4     Out2 -> Pc4ToOut2
    Mix DAC4     Out4 -> Pc4ToOut4
    Mix DAC6     Out1 -> Pc6ToOut1
    Mix DAC6     Out3 -> Pc6ToOut3
    Mix DAC6     Out2 -> Pc6ToOut2
    Mix DAC6     Out4 -> Pc6ToOut4
    Mix DAC8     Out1 -> Pc8ToOut1
    Mix DAC8     Out3 -> Pc8ToOut3
    Mix DAC8     Out2 -> Pc8ToOut2
    Mix DAC8     Out4 -> Pc8ToOut4
    Mix In1      Out1 -> In1ToOut1
    Mix In1      Out3 -> In1ToOut3
    Mix In1      Out2 -> In1ToOut2
    Mix In1      Out4 -> In1ToOut4
    Mix In3      Out1 -> In3ToOut1
    Mix In3      Out3 -> In3ToOut3
    Mix In3      Out2 -> In3ToOut2
    Mix In3      Out4 -> In3ToOut4
    Mix SpdifIn1 Out1 -> Spdif1ToOut1
    Mix SpdifIn1 Out3 -> Spdif1ToOut3
    Mix SpdifIn1 Out2 -> Spdif1ToOut2
    Mix SpdifIn1 Out4 -> Spdif1ToOut4
    Mix In2      Out1 -> In2ToOut1
    Mix In2      Out3 -> In2ToOut3
    Mix In2      Out2 -> In2ToOut2
    Mix In2      Out4 -> In2ToOut4
    Mix In4      Out1 -> In4ToOut1
    Mix In4      Out3 -> In4ToOut3
    Mix In4      Out2 -> In4ToOut2
    Mix In4      Out4 -> In4ToOut4
    Mix SpdifIn2 Out1 -> Spdif2ToOut1
    Mix SpdifIn2 Out3 -> Spdif2ToOut3
    Mix SpdifIn2 Out2 -> Spdif2ToOut2
    Mix SpdifIn2 Out4 -> Spdif2ToOut4
    Mix inp      out  -> error $ "Unsupported mix: " +|| inp ||+ " -> " +|| out ||+ ""

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
    { _mix              :: Map OutChannel (Map InChannel MixValue)
    -- TODO: Add support for 96k (high res) mixer
    , _in3Gain          :: Bool
    , _in4Gain          :: Bool
    , _out12Opts        :: OutOpts
    , _out34Opts        :: OutOpts
    , _out56Opts        :: OutOpts
    , _out12ToSpdif     :: Bool
    , _midiThru         :: Bool
    , _spdifTransparent :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   MixerState where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON MixerState where parseJSON = genericParseJSON stripLensPrefix

instance Default MixerState where
    def = MixerState
        { _mix = mempty
        , _in3Gain = False
        , _in4Gain = False
        , _out12Opts = def
        , _out34Opts = def
        , _out56Opts = def
        , _out12ToSpdif = False
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
    , (Out12ToSpdifOut,   booleanControl out12ToSpdif)
    , (Midithru,          booleanControl midiThru)
    , (SpdifTransparent,  booleanControl spdifTransparent)
    , (Pc1ToOut1,         mixControl (Mix DAC1 Out1))
    , (Pc1ToOut3,         mixControl (Mix DAC1 Out3))
    , (Pc1ToOut2,         mixControl (Mix DAC1 Out2))
    , (Pc1ToOut4,         mixControl (Mix DAC1 Out4))
    , (Pc3ToOut1,         mixControl (Mix DAC3 Out1))
    , (Pc3ToOut3,         mixControl (Mix DAC3 Out3))
    , (Pc3ToOut2,         mixControl (Mix DAC3 Out2))
    , (Pc3ToOut4,         mixControl (Mix DAC3 Out4))
    , (Pc5ToOut1,         mixControl (Mix DAC5 Out1))
    , (Pc5ToOut3,         mixControl (Mix DAC5 Out3))
    , (Pc5ToOut2,         mixControl (Mix DAC5 Out2))
    , (Pc5ToOut4,         mixControl (Mix DAC5 Out4))
    , (Pc7ToOut1,         mixControl (Mix DAC7 Out1))
    , (Pc7ToOut3,         mixControl (Mix DAC7 Out3))
    , (Pc7ToOut2,         mixControl (Mix DAC7 Out2))
    , (Pc7ToOut4,         mixControl (Mix DAC7 Out4))
    , (Pc2ToOut1,         mixControl (Mix DAC2 Out1))
    , (Pc2ToOut3,         mixControl (Mix DAC2 Out3))
    , (Pc2ToOut2,         mixControl (Mix DAC2 Out2))
    , (Pc2ToOut4,         mixControl (Mix DAC2 Out4))
    , (Pc4ToOut1,         mixControl (Mix DAC4 Out1))
    , (Pc4ToOut3,         mixControl (Mix DAC4 Out3))
    , (Pc4ToOut2,         mixControl (Mix DAC4 Out2))
    , (Pc4ToOut4,         mixControl (Mix DAC4 Out4))
    , (Pc6ToOut1,         mixControl (Mix DAC6 Out1))
    , (Pc6ToOut3,         mixControl (Mix DAC6 Out3))
    , (Pc6ToOut2,         mixControl (Mix DAC6 Out2))
    , (Pc6ToOut4,         mixControl (Mix DAC6 Out4))
    , (Pc8ToOut1,         mixControl (Mix DAC8 Out1))
    , (Pc8ToOut3,         mixControl (Mix DAC8 Out3))
    , (Pc8ToOut2,         mixControl (Mix DAC8 Out2))
    , (Pc8ToOut4,         mixControl (Mix DAC8 Out4))
    , (In1ToOut1,         mixControl (Mix In1 Out1))
    , (In1ToOut3,         mixControl (Mix In1 Out3))
    , (In1ToOut2,         mixControl (Mix In1 Out2))
    , (In1ToOut4,         mixControl (Mix In1 Out4))
    , (In3ToOut1,         mixControl (Mix In3 Out1))
    , (In3ToOut3,         mixControl (Mix In3 Out3))
    , (In3ToOut2,         mixControl (Mix In3 Out2))
    , (In3ToOut4,         mixControl (Mix In3 Out4))
    , (Spdif1ToOut1,      mixControl (Mix SpdifIn1 Out1))
    , (Spdif1ToOut3,      mixControl (Mix SpdifIn1 Out3))
    , (Spdif1ToOut2,      mixControl (Mix SpdifIn1 Out2))
    , (Spdif1ToOut4,      mixControl (Mix SpdifIn1 Out4))
    , (In2ToOut1,         mixControl (Mix In2 Out1))
    , (In2ToOut3,         mixControl (Mix In2 Out3))
    , (In2ToOut2,         mixControl (Mix In2 Out2))
    , (In2ToOut4,         mixControl (Mix In2 Out4))
    , (In4ToOut1,         mixControl (Mix In4 Out1))
    , (In4ToOut3,         mixControl (Mix In4 Out3))
    , (In4ToOut2,         mixControl (Mix In4 Out2))
    , (In4ToOut4,         mixControl (Mix In4 Out4))
    , (Spdif2ToOut1,      mixControl (Mix SpdifIn2 Out1))
    , (Spdif2ToOut3,      mixControl (Mix SpdifIn2 Out3))
    , (Spdif2ToOut2,      mixControl (Mix SpdifIn2 Out2))
    , (Spdif2ToOut4,      mixControl (Mix SpdifIn2 Out4))
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
    mixControl :: Mix -> (RawControlValue -> MixerState -> MixerState, MixerState -> RawControlValue)
    mixControl (Mix inp out) = (mixControlR, mixControlW) where
        mixControlR value = mix . at out . non mempty . at inp  ?~ (fromIntegral value / 0x7fff)
        mixControlW state = floor $ (state ^. mix . at out . non mempty . at inp . non 0) * 0x7fff

allControls :: [RawControl]
allControls = fst <$> mixerControls
