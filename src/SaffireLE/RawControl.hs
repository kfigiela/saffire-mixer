{-# LANGUAGE FlexibleContexts #-}

module SaffireLE.RawControl where

import           Universum

import           Data.List        (lookup)
import           Universum.Unsafe (fromJust)


type RawControlId    = Word32
type RawControlValue = Word32

-- | Enum type for all parameters that may be retrived/set on Saffire LE
data RawControl =
      Pc1ToOut1
    | Pc1ToOut3
    | Pc1ToOut2
    | Pc1ToOut4
    | Pc3ToOut1
    | Pc3ToOut3
    | Pc3ToOut2
    | Pc3ToOut4
    | Pc5ToOut1
    | Pc5ToOut3
    | Pc5ToOut2
    | Pc5ToOut4
    | Pc7ToOut1
    | Pc7ToOut3
    | Pc7ToOut2
    | Pc7ToOut4
    | Pc2ToOut1
    | Pc2ToOut3
    | Pc2ToOut2
    | Pc2ToOut4
    | Pc4ToOut1
    | Pc4ToOut3
    | Pc4ToOut2
    | Pc4ToOut4
    | Pc6ToOut1
    | Pc6ToOut3
    | Pc6ToOut2
    | Pc6ToOut4
    | Pc8ToOut1
    | Pc8ToOut3
    | Pc8ToOut2
    | Pc8ToOut4
    | In1ToOut1
    | In1ToOut3
    | In1ToOut2
    | In1ToOut4
    | In3ToOut1
    | In3ToOut3
    | In3ToOut2
    | In3ToOut4
    | Spdif1ToOut1
    | Spdif1ToOut3
    | Spdif1ToOut2
    | Spdif1ToOut4
    | In2ToOut1
    | In2ToOut3
    | In2ToOut2
    | In2ToOut4
    | In4ToOut1
    | In4ToOut3
    | In4ToOut2
    | In4ToOut4
    | Spdif2ToOut1
    | Spdif2ToOut3
    | Spdif2ToOut2
    | Spdif2ToOut4
    --
    | Out12ToSpdifOut

    -- Mixer 96k
    -- - RecMix
    | In1ToRecmix_96K
    | In3ToRecmix_96K
    | Spdif1ToRecmix_96K
    | In2ToRecmix_96K
    | In4ToRecmix_96K
    | Spdif2ToRecmix_96K
    -- Out1
    | RecmixToOut1_96K
    | Pc1ToOut1_96K
    | Pc2ToOut1_96K
    -- Out3
    | RecmixToOut3_96K
    | Pc1ToOut3_96K
    | Pc2ToOut3_96K
    -- Out2
    | RecmixToOut2_96K
    | Pc1ToOut2_96K
    | Pc2ToOut2_96K
    -- Out 4
    | RecmixToOut4_96K
    | Pc1ToOut4_96K
    | Pc2ToOut4_96K
    --
    | Out12ToSpdifOut_96K

    -- Metering
    | MeteringIn1
    | MeteringIn3
    | MeteringSpdif1
    | MeteringIn2
    | MeteringIn4
    | MeteringSpdif2
    | MeteringOut1
    | MeteringOut3
    | MeteringOut5
    | MeteringOut7
    | MeteringOut2
    | MeteringOut4
    | MeteringOut6
    | MeteringOut8
    | MeteringPc1
    | MeteringPc3
    | MeteringPc2
    | MeteringPc4
    --
    | HighGainLine3
    | HighGainLine4
    | BitfieldOut12 -- Attenuation level + mute bit flag
    | BitfieldOut34 -- Attenuation level + mute bit flag
    | BitfieldOut56 -- Attenuation level + mute bit flag
    | ExtClockLock -- ^ Indicates if DAC is locked on external SPDIF clock
    | AudioOn -- ^ Indicates if there is audio playing from computer
    | SaveSettings
    | Midithru
    | SpdifTransparent -- ^ SPDIF pass-thru
    deriving (Show, Eq, Ord)

fromRawControlEnum :: HasCallStack => RawControl -> RawControlId
fromRawControlEnum control = fromMaybe (error $ "fromRawControlEnum " <> show control) $ lookup control controlsTable

toRawControlEnum :: HasCallStack => RawControlId -> Maybe RawControl
toRawControlEnum control = lookup control (map swap controlsTable)

-- | Encodes mapping between control enum and control id
controlsTable :: [(RawControl, RawControlId)]
controlsTable =
    [ (Pc1ToOut1,             0)
    , (Pc1ToOut3,             1)
    , (Pc1ToOut2,             2)
    , (Pc1ToOut4,             3)
    , (Pc3ToOut1,             4)
    , (Pc3ToOut3,             5)
    , (Pc3ToOut2,             6)
    , (Pc3ToOut4,             7)
    , (Pc5ToOut1,             8)
    , (Pc5ToOut3,             9)
    , (Pc5ToOut2,             10)
    , (Pc5ToOut4,             11)
    , (Pc7ToOut1,             12)
    , (Pc7ToOut3,             13)
    , (Pc7ToOut2,             14)
    , (Pc7ToOut4,             15)
    , (Pc2ToOut1,             16)
    , (Pc2ToOut3,             17)
    , (Pc2ToOut2,             18)
    , (Pc2ToOut4,             19)
    , (Pc4ToOut1,             20)
    , (Pc4ToOut3,             21)
    , (Pc4ToOut2,             22)
    , (Pc4ToOut4,             23)
    , (Pc6ToOut1,             24)
    , (Pc6ToOut3,             25)
    , (Pc6ToOut2,             26)
    , (Pc6ToOut4,             27)
    , (Pc8ToOut1,             28)
    , (Pc8ToOut3,             29)
    , (Pc8ToOut2,             30)
    , (Pc8ToOut4,             31)
    , (In1ToOut1,             32)
    , (In1ToOut3,             33)
    , (In1ToOut2,             34)
    , (In1ToOut4,             35)
    , (In3ToOut1,             36)
    , (In3ToOut3,             37)
    , (In3ToOut2,             38)
    , (In3ToOut4,             39)
    , (Spdif1ToOut1,          40)
    , (Spdif1ToOut3,          41)
    , (Spdif1ToOut2,          42)
    , (Spdif1ToOut4,          43)
    , (In2ToOut1,             44)
    , (In2ToOut3,             45)
    , (In2ToOut2,             46)
    , (In2ToOut4,             47)
    , (In4ToOut1,             48)
    , (In4ToOut3,             49)
    , (In4ToOut2,             50)
    , (In4ToOut4,             51)
    , (Spdif2ToOut1,          52)
    , (Spdif2ToOut3,          53)
    , (Spdif2ToOut2,          54)
    , (Spdif2ToOut4,          55)

    , (Out12ToSpdifOut,       64)

    -- 96kHz controls
    , (In1ToRecmix_96K,        66)
    , (In3ToRecmix_96K,        67)
    , (Spdif1ToRecmix_96K,     68)
    , (In2ToRecmix_96K,        69)
    , (In4ToRecmix_96K,        70)
    , (Spdif2ToRecmix_96K,     71)

    , (RecmixToOut1_96K,       72)
    , (Pc1ToOut1_96K,          73)
    , (Pc2ToOut1_96K,          74)
    , (RecmixToOut3_96K,       75)
    , (Pc1ToOut3_96K,          76)
    , (Pc2ToOut3_96K,          77)
    , (RecmixToOut2_96K,       78)
    , (Pc1ToOut2_96K,          79)
    , (Pc2ToOut2_96K,          80)
    , (RecmixToOut4_96K,       81)
    , (Pc1ToOut4_96K,          82)
    , (Pc2ToOut4_96K,          83)

    , (Out12ToSpdifOut_96K,    84)

    -- // metering
    , (MeteringIn1,           90)
    , (MeteringIn3,           91)
    , (MeteringSpdif1,        92)
    , (MeteringIn2,           93)
    , (MeteringIn4,           94)
    , (MeteringSpdif2,        95)

    , (MeteringOut1,          96)
    , (MeteringOut2,          98)
    , (MeteringOut3,          97)
    , (MeteringOut4,          99)
    , (MeteringOut5,          100)
    , (MeteringOut6,          102)
    , (MeteringOut7,          101)
    , (MeteringOut8,          103)

    , (MeteringPc1,           104)
    , (MeteringPc3,           105)
    , (MeteringPc2,           106)
    , (MeteringPc4,           107)

    -- // other stuff
    , (HighGainLine3,         85)
    , (HighGainLine4,         86)

    , (BitfieldOut12,         87)
    , (BitfieldOut34,         88)
    , (BitfieldOut56,         89)

    , (ExtClockLock,          108)
    , (AudioOn,               109)
    , (SaveSettings,          110)
    , (Midithru,              111)
    , (SpdifTransparent,      112)
    ]
