module SaffireLE.RawControl where

import Universum

import Universum.Unsafe (fromJust)
import Data.List (lookup)

type RawControlId = Word32
type RawControlValue = Word32

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
    -- ??
    | Out1ToSpdifOut1
    | Out2ToSpdifOut2

    -- Mixer 96k
    | In1ToRecmix96K
    | In3ToRecmix96K
    | Spdif1ToRecmix96K
    | In2ToRecmix96K
    | In4ToRecmix96K
    | Spdif2ToRecmix96K
    | RecmixToOut196K
    | Pc1ToOut196K
    | Pc2ToOut196K
    | RecmixToOut396K
    | Pc1ToOut396K
    | Pc2ToOut396K
    | RecmixToOut296K
    | Pc1ToOut296K
    | Pc2ToOut296K
    | RecmixToOut496K
    | Pc1ToOut496K
    | Pc2ToOut496K
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
    | BitfieldOut12
    | BitfieldOut34
    | BitfieldOut56
    | ExtClockLock
    | AudioOn
    | SaveSettings
    | Midithru
    | SpdifTransparent
    deriving (Show, Eq, Ord)

fromRawControlEnum :: RawControl -> RawControlId
fromRawControlEnum = fromJust . flip lookup controlsTable

toRawControlEnum :: RawControlId -> RawControl
toRawControlEnum = fromJust . flip lookup (map swap controlsTable)

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

    , (Out1ToSpdifOut1,       64)

    -- // 96kHz controls
    , (In1ToRecmix96K,        66)
    , (In3ToRecmix96K,        67)
    , (Spdif1ToRecmix96K,     68)
    , (In2ToRecmix96K,        69)
    , (In4ToRecmix96K,        70)
    , (Spdif2ToRecmix96K,     71)

    , (RecmixToOut196K,       72)
    , (Pc1ToOut196K,          73)
    , (Pc2ToOut196K,          74)
    , (RecmixToOut396K,       75)
    , (Pc1ToOut396K,          76)
    , (Pc2ToOut396K,          77)
    , (RecmixToOut296K,       78)
    , (Pc1ToOut296K,          79)
    , (Pc2ToOut296K,          80)
    , (RecmixToOut496K,       81)
    , (Pc1ToOut496K,          82)
    , (Pc2ToOut496K,          83)

    , (Out2ToSpdifOut2,       84)

    -- // metering
    , (MeteringIn1,           90)
    , (MeteringIn3,           91)
    , (MeteringSpdif1,        92)
    , (MeteringIn2,           93)
    , (MeteringIn4,           94)
    , (MeteringSpdif2,        95)

    , (MeteringOut1,          96)
    , (MeteringOut3,          97)
    , (MeteringOut5,          98)
    , (MeteringOut7,          99)
    , (MeteringOut2,          100)
    , (MeteringOut4,          101)
    , (MeteringOut6,          102)
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

data BitField =
      BitfieldBitDim
    | BitfieldBitMute
    | BitfieldBitDacignore
    | BitfieldBitHwctrl
    | BitfieldBitDac
    deriving (Show, Eq)

fromBitFieldEnum :: BitField -> RawControlValue
fromBitFieldEnum = fromJust . flip lookup bitFieldTable

toBitFieldEnum :: RawControlValue -> BitField
toBitFieldEnum = fromJust . flip lookup (map swap bitFieldTable)

bitFieldTable :: [(BitField, RawControlValue)]
bitFieldTable =
    [ (BitfieldBitDim,        24)
    , (BitfieldBitMute,       25)
    , (BitfieldBitDacignore,  26)
    , (BitfieldBitHwctrl,     27)
    , (BitfieldBitDac,        0)
    ]
