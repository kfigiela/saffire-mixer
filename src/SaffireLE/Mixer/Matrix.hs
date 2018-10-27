{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.Matrix where

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

-- | 44.1 kHz and 48 kHz sample rate mixers
data MatrixMixer
    = MatrixMixer
    { _out1         :: Mix
    , _out2         :: Mix
    , _out3         :: Mix
    , _out4         :: Mix
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   MatrixMixer where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON MatrixMixer where parseJSON = genericParseJSON stripLensPrefix
instance Default  MatrixMixer where def       = MatrixMixer def def def def False

data Mix
    = Mix
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
    } deriving (Show, Eq, Generic)

instance ToJSON   Mix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON Mix where parseJSON = genericParseJSON stripLensPrefix
instance Default  Mix where def = Mix def def def def def def def def def def def def def def


makeFieldsNoPrefix ''MatrixMixer
makeFieldsNoPrefix ''Mix
