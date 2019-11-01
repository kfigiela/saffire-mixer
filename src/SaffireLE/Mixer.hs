module SaffireLE.Mixer where

import           Universum

import           Data.Aeson             (FromJSON, ToJSON, parseJSON, toJSON)
import qualified SaffireLE.Mixer.Raw    as R
import qualified SaffireLE.Mixer.Stereo as S

data Mixer
    = Raw R.MixerState
    | Stereo S.MixerState

instance FromJSON Mixer where
    parseJSON v = (Raw <$> parseJSON v) <|> (Stereo <$> parseJSON v)

instance ToJSON Mixer where
    toJSON (Raw m)    = toJSON m
    toJSON (Stereo m) = toJSON m

toRaw :: Mixer -> R.MixerState
toRaw (Raw m)    = m
toRaw (Stereo m) = S.toRaw m

toStereo :: Mixer -> S.MixerState
toStereo (Raw m)    = S.fromRaw m
toStereo (Stereo m) = m
