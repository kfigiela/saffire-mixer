{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module SaffireLE.Status where

import           Universum

import           Control.Lens         (at)
import           Control.Lens.TH      (makeLenses)
import           Data.Aeson           (ToJSON)
import           Data.Aeson.Extra     (StripLensPrefix (..))
import           Data.Default.Class   (Default, def)
import qualified Data.Map             as Map

import           SaffireLE.RawControl (RawControl (..), RawControlValue)
import           SaffireLE.Utils      (toBool)

data VUMeter
    = In1
    | In3
    | SpdifIn1
    | In2
    | In4
    | SpdifIn2
    | Out1
    | Out3
    | Out5
    | SpdifOut7
    | Out2
    | Out4
    | Out6
    | SpdifOut8
    | DAC1
    | DAC3
    | DAC2
    | DAC4
    deriving (Show, Eq, Generic, ToJSON)

type MeterValue = Double

data VUMeters
    = VUMeters
    { _in1       :: MeterValue
    , _in3       :: MeterValue
    , _spdifIn1  :: MeterValue
    , _in2       :: MeterValue
    , _in4       :: MeterValue
    , _spdifIn2  :: MeterValue
    , _out1      :: MeterValue
    , _out3      :: MeterValue
    , _out5      :: MeterValue
    , _spdifOut7 :: MeterValue
    , _out2      :: MeterValue
    , _out4      :: MeterValue
    , _out6      :: MeterValue
    , _spdifOut8 :: MeterValue
    , _dac1      :: MeterValue
    , _dac3      :: MeterValue
    , _dac2      :: MeterValue
    , _dac4      :: MeterValue
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON) via (StripLensPrefix VUMeters)

instance Default VUMeters where
    def = VUMeters
        { _in1       = nan
        , _in3       = nan
        , _spdifIn1  = nan
        , _in2       = nan
        , _in4       = nan
        , _spdifIn2  = nan
        , _out1      = nan
        , _out3      = nan
        , _out5      = nan
        , _spdifOut7 = nan
        , _out2      = nan
        , _out4      = nan
        , _out6      = nan
        , _spdifOut8 = nan
        , _dac1      = nan
        , _dac3      = nan
        , _dac2      = nan
        , _dac4      = nan
        }
        where nan = 0.0/0.0

makeLenses ''VUMeters

data AudioStatus = Idle | Running | NotConnected deriving (Show, Eq, Generic, ToJSON, Enum)
instance Default AudioStatus where def = NotConnected

data ExternalClockStatus
    = NoSignal
    | Locked -- ^ not sure
    | Signal
    deriving (Show, Eq, Generic, ToJSON, Enum)
instance Default ExternalClockStatus where def = NoSignal

data DeviceStatus
    = DeviceStatus
    { _meters       :: VUMeters
    , _extClockLock :: ExternalClockStatus
    , _audioOn      :: AudioStatus
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON) via (StripLensPrefix DeviceStatus)

makeLenses ''DeviceStatus

updateDeviceStatus :: [(RawControl, RawControlValue)] -> DeviceStatus
updateDeviceStatus = foldr (flip updateDeviceStatus') def

updateDeviceStatus' :: DeviceStatus -> (RawControl, RawControlValue) -> DeviceStatus
updateDeviceStatus' state (control, value) = f value state where
    f = fromMaybe (const id) $ Map.lookup control meterControlsMap

dbValue :: Word32 -> MeterValue
dbValue val = 20 * (logBase 10 (fromIntegral val / 0x7fffffff))

meterControlsMap :: Map RawControl ( RawControlValue -> DeviceStatus -> DeviceStatus)
meterControlsMap = Map.fromList meterControls

meterControls :: [(RawControl, RawControlValue -> DeviceStatus -> DeviceStatus)]
meterControls =
    [ (MeteringIn1,    channelMeter in1)
    , (MeteringIn3,    channelMeter in3)
    , (MeteringSpdif1, channelMeter spdifIn1)
    , (MeteringIn2,    channelMeter in2)
    , (MeteringIn4,    channelMeter in4)
    , (MeteringSpdif2, channelMeter spdifIn2)
    , (MeteringOut1,   channelMeter out1)
    , (MeteringOut3,   channelMeter out3)
    , (MeteringOut5,   channelMeter out5)
    , (MeteringOut7,   channelMeter spdifOut7)
    , (MeteringOut2,   channelMeter out2)
    , (MeteringOut4,   channelMeter out4)
    , (MeteringOut6,   channelMeter out6)
    , (MeteringOut8,   channelMeter spdifOut8)
    , (MeteringPc1,    channelMeter dac1)
    , (MeteringPc3,    channelMeter dac3)
    , (MeteringPc2,    channelMeter dac2)
    , (MeteringPc4,    channelMeter dac4)
    , (ExtClockLock,   enumControl extClockLock)
    , (AudioOn,        enumControl audioOn)
    ]
  where
    booleanControl :: Lens' DeviceStatus Bool -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    booleanControl l value = l .~ toBool value
    wordControl :: Lens' DeviceStatus Word32 -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    wordControl l value = l .~  value
    enumControl :: Enum a => Lens' DeviceStatus a -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    enumControl l value = l .~ toEnum (fromIntegral value)
    channelMeter :: Lens' VUMeters MeterValue -> (RawControlValue -> DeviceStatus -> DeviceStatus)
    channelMeter meter value = meters . meter .~ dbValue value

allMeters :: [RawControl]
allMeters = fst <$> meterControls

vuMeters :: [(VUMeter, VUMeters -> MeterValue)]
vuMeters =
    [ (In1,       view in1)
    , (In2,       view in2)
    , (In3,       view in3)
    , (In4,       view in4)
    , (SpdifIn1,  view spdifIn1)
    , (SpdifIn2,  view spdifIn2)
    , (DAC1,      view dac1)
    , (DAC2,      view dac2)
    , (DAC3,      view dac3)
    , (DAC4,      view dac4)
    , (Out1,      view out1)
    , (Out2,      view out2)
    , (Out3,      view out3)
    , (Out4,      view out4)
    , (Out5,      view out5)
    , (Out6,      view out6)
    , (SpdifOut7, view spdifOut7)
    , (SpdifOut8, view spdifOut8)
    ]
