{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SaffireLE.Mixer.Stereo where

import           Universum

import           Control.Lens                  (at)
import           Control.Lens.TH               (makeLenses)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Aeson.Extra              (StripLensPrefix (..))
import           Data.Bits.Lens                (bitAt, byteAt)
import           Data.Default.Class            (Default, def)
import qualified Data.Map                      as Map

import           SaffireLE.Mixer.Raw           (OutOpts)
import qualified SaffireLE.Mixer.Raw           as R
import qualified SaffireLE.Mixer.Stereo.HiRes  as H
import qualified SaffireLE.Mixer.Stereo.LowRes as L
import           SaffireLE.RawControl          (RawControl (..), RawControlValue)
import           SaffireLE.Utils               (toBool)



data MixerState
    = MixerState
    { _lowResMixer      :: L.StereoMixer
    , _highResMixer     :: H.StereoMixer
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

fromRaw :: R.MixerState -> MixerState
fromRaw raw =
    MixerState
    { _lowResMixer      = L.toStereoMixer $ raw ^. R.lowResMixer
    , _highResMixer     = H.toStereoMixer $ raw ^. R.highResMixer
    , _in3Gain          = raw ^. R.in3Gain
    , _in4Gain          = raw ^. R.in4Gain
    , _out12Opts        = raw ^. R.out12Opts
    , _out34Opts        = raw ^. R.out34Opts
    , _out56Opts        = raw ^. R.out56Opts
    , _midiThru         = raw ^. R.midiThru
    , _spdifTransparent = raw ^. R.spdifTransparent
    }

toRaw :: MixerState -> R.MixerState
toRaw mix =
    R.MixerState
    { R._lowResMixer      = L.fromStereoMixer $ mix ^. lowResMixer
    , R._highResMixer     = H.fromStereoMixer $ mix ^. highResMixer
    , R._in3Gain          = mix ^. in3Gain
    , R._in4Gain          = mix ^. in4Gain
    , R._out12Opts        = mix ^. out12Opts
    , R._out34Opts        = mix ^. out34Opts
    , R._out56Opts        = mix ^. out56Opts
    , R._midiThru         = mix ^. midiThru
    , R._spdifTransparent = mix ^. spdifTransparent
    }
