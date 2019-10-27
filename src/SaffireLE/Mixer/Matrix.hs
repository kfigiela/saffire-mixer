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

import           Control.Lens.TH    (makeFieldsNoPrefix)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Aeson.Extra   (StripLensPrefix (..))
import           Data.Default.Class (Default, def)
import           Universum

type MixValue = Double

-- | 44.1 kHz and 48 kHz sample rate mixers
data MatrixMixer
    = MatrixMixer
    { _out1         :: Mix
    , _out2         :: Mix
    , _out3         :: Mix
    , _out4         :: Mix
    , _out12ToSpdif :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (StripLensPrefix MatrixMixer)
instance Default  MatrixMixer where def = MatrixMixer def def def def False

data Mix
    = Mix
    { _dac1   :: MixValue
    , _dac2   :: MixValue
    , _dac3   :: MixValue
    , _dac4   :: MixValue
    , _dac5   :: MixValue
    , _dac6   :: MixValue
    , _dac7   :: MixValue
    , _dac8   :: MixValue
    , _in1    :: MixValue
    , _in2    :: MixValue
    , _in3    :: MixValue
    , _in4    :: MixValue
    , _spdif1 :: MixValue
    , _spdif2 :: MixValue
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix Mix)

makeFieldsNoPrefix ''MatrixMixer
makeFieldsNoPrefix ''Mix
