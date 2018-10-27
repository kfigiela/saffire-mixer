{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.HiRes where

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

-- | 88.2 kHz and 96 kHz sample rate mixers
data Mixer
    = Mixer
    { _out1         :: LMix
    , _out2         :: RMix
    , _out3         :: LMix
    , _out4         :: RMix
    , _recMix       :: RecMix
    , _out12ToSpdif :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON   Mixer where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON Mixer where parseJSON = genericParseJSON stripLensPrefix
instance Default  Mixer where def = Mixer def def def def def False

data LMix
    = LMix
    { _dac1   :: MixValue
    , _dac3   :: MixValue
    , _recMix :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   LMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON LMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  LMix where def = LMix def def def

data RMix
    = RMix
    { _dac2   :: MixValue
    , _dac4   :: MixValue
    , _recMix :: MixValue
    } deriving (Show, Eq, Generic)

instance ToJSON   RMix where    toJSON = genericToJSON    stripLensPrefix
instance FromJSON RMix where parseJSON = genericParseJSON stripLensPrefix
instance Default  RMix where def = RMix def def def

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


makeFieldsNoPrefix ''Mixer
makeFieldsNoPrefix ''LMix
makeFieldsNoPrefix ''RMix
makeFieldsNoPrefix ''RecMix
