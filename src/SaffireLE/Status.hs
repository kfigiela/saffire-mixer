{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module SaffireLE.Status where

import           Universum

import           Control.Lens         (at, non, (?~), Getter, to)
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

import           SaffireLE.Mixer      (InChannel (..), OutChannel (..))
import           SaffireLE.RawControl (RawControl (..), RawControlValue)
import           SaffireLE.Utils      (toBool)

data Meter = InMeter InChannel | OutMeter OutChannel deriving (Show, Eq, Generic, Ord, ToJSON, FromJSON)
instance ToJSONKey Meter where toJSONKey = toJSONKeyText show

name :: Getter Meter Text
name = to $ \case
    InMeter ch -> show ch
    OutMeter ch -> show ch

type MeterValue = Double

data DeviceStatus
    = DeviceStatus
    { _meters       :: Map Meter MeterValue
    , _extClockLock :: Word32
    , _audioOn      :: Bool
    } deriving (Show, Eq, Generic)

instance Default DeviceStatus where
    def = DeviceStatus def 0 False

instance ToJSON   DeviceStatus where    toJSON = genericToJSON    stripLensPrefix
-- instance FromJSON DeviceStatus where parseJSON = genericParseJSON stripLensPrefix

makeLenses ''DeviceStatus

updateDeviceStatus :: DeviceStatus -> [(RawControl, RawControlValue)] -> DeviceStatus
updateDeviceStatus = foldl updateDeviceStatus'

updateDeviceStatus' :: DeviceStatus -> (RawControl, RawControlValue) -> DeviceStatus
updateDeviceStatus' state (control, value) = f value state where
    f = fromMaybe (const identity) $ meterControlsMap ^. at control
    meterControlsMap = Map.fromList meterControls

dbValue :: Word32 -> MeterValue
dbValue val = 20 * (logBase 10 (fromIntegral val / 0x7fffffff))

meterControls :: [(RawControl, RawControlValue -> DeviceStatus -> DeviceStatus)]
meterControls =
    [ (MeteringIn1,    channelMeter (InMeter In1))
    , (MeteringIn3,    channelMeter (InMeter In3))
    , (MeteringSpdif1, channelMeter (InMeter SpdifIn1))
    , (MeteringIn2,    channelMeter (InMeter In2))
    , (MeteringIn4,    channelMeter (InMeter In4))
    , (MeteringSpdif2, channelMeter (InMeter SpdifIn2))
    , (MeteringOut1,   channelMeter (OutMeter Out1))
    , (MeteringOut3,   channelMeter (OutMeter Out3))
    , (MeteringOut5,   channelMeter (OutMeter Out5))
    , (MeteringOut7,   channelMeter (OutMeter SpdifOut7))
    , (MeteringOut2,   channelMeter (OutMeter Out2))
    , (MeteringOut4,   channelMeter (OutMeter Out4))
    , (MeteringOut6,   channelMeter (OutMeter Out6))
    , (MeteringOut8,   channelMeter (OutMeter SpdifOut8))
    , (MeteringPc1,    channelMeter (InMeter DAC1))
    , (MeteringPc3,    channelMeter (InMeter DAC3))
    , (MeteringPc2,    channelMeter (InMeter DAC2))
    , (MeteringPc4,    channelMeter (InMeter DAC4))
    , (ExtClockLock, wordControl extClockLock)
    , (AudioOn, booleanControl audioOn)
    ]
  where
    booleanControl :: Lens' DeviceStatus Bool -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    booleanControl l value = l .~ toBool value
    wordControl :: Lens' DeviceStatus Word32 -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    wordControl l value = l .~  value
    channelMeter :: Meter -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    channelMeter meter value = meters . at meter ?~ dbValue value

allMeters :: [RawControl]
allMeters = fst <$> meterControls
