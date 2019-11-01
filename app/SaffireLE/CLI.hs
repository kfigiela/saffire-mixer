{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}

module SaffireLE.CLI where

import           Prelude                      (isInfinite)
import           Universum

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (AsyncException (..), handle, throw)
import           Control.Lens                 (ix)
import qualified Data.ByteString.Char8        as BS
import           Data.Default.Class           (def)
import qualified Data.Text                    as Text
import           Data.Yaml                    (decodeFileThrow)
import           Data.Yaml.Pretty
import           Fmt
import           Fmt.Terminal                 (Color (..), colorF, colorVividF)
import           Options.Applicative
import           System.Console.ANSI
import qualified System.Console.Terminal.Size as Terminal (Window (..), size)

import           SaffireLE.Device
import qualified SaffireLE.Mixer              as M
import           SaffireLE.Mixer.Raw
import qualified SaffireLE.Mixer.Stereo       as SM
import qualified SaffireLE.RawControl         as Raw
import           SaffireLE.Server             (runServer, statePath)
import           SaffireLE.Status

data Mode = Matrix | Stereo deriving (Eq, Show)

data Command
  = LoadSettings { _filename :: FilePath }
  | ReadSettings { _filename :: FilePath, _stereo :: Mode }
  | SaveSettings
  | Status
  | Meter
  | Server
  deriving (Eq, Show)

loadSettings :: FilePath -> Parser Command
loadSettings defaultSettingsPath =
    LoadSettings
    <$> strOption (long "file" <> short 'f' <> help "File where mixer state is stored" <> metavar "FILE" <> value defaultSettingsPath <> showDefault)

readSettings :: FilePath -> Parser Command
readSettings defaultSettingsPath =
    ReadSettings
    <$> strOption (long "file" <> short 'f' <> help "File where mixer state is stored" <> metavar "FILE" <> value defaultSettingsPath <> showDefault)
    <*> flag Stereo Matrix (long "matrix" <> short 'm' <> help "Use matrix format")

cli :: FilePath -> Parser Command
cli defaultSettingsPath = hsubparser
   (  command "load"  (info (loadSettings defaultSettingsPath <**> helper) (progDesc "Load settings from file to device"))
   <> command "read"  (info (readSettings defaultSettingsPath <**> helper) (progDesc "Read settings from device to file"))
   <> command "save"  (info (pure SaveSettings) (progDesc "Store settings in a persistent device memory. Causes brief audio interruption."))
   <> command "status" (info (pure Status) (progDesc "Display device status incl. metering values"))
   <> command "meter" (info (pure Meter) (progDesc "Display bar meters"))
   <> command "server" (info (pure Server) (progDesc "Display bar meters"))
   )

main :: IO ()
main = do
    defaultSettingsPath <- statePath
    execParser (opts defaultSettingsPath) >>= runCommands

opts :: FilePath -> ParserInfo Command
opts defaultSettingsPath = info (cli defaultSettingsPath <**> helper) idm

barRange :: Double
barRange = 80

runCommands :: Command -> IO ()
runCommands cmd =
    case cmd of
        LoadSettings file -> runDevice $ \devPtr -> do
            mixerState <- M.toRaw <$> decodeFileThrow file
            let rawData = hardwarizeMixerState mixerState
            void $ writeSaffire devPtr rawData
        ReadSettings file Matrix -> runDevice $ \devPtr -> do
            rawData <- readSaffire devPtr allControls
            let mixer' = M.Raw $ updateMixerState def rawData
                yaml = encodePretty (defConfig & setConfCompare compare) mixer'
            void $ BS.writeFile file yaml
        ReadSettings file Stereo -> runDevice $ \devPtr -> do
            rawData <- readSaffire devPtr allControls
            let mixer' = M.Stereo $ SM.fromRaw $ updateMixerState def rawData
                yaml = encodePretty (defConfig & setConfCompare compare) mixer'
            void $ BS.writeFile file yaml
        SaveSettings -> runDevice $ \devPtr -> void $ writeSaffire devPtr [(Raw.SaveSettings, 1)]
        Status -> runDevice $ \devPtr -> do
            rawData <- readSaffire devPtr allMeters
            let status = updateDeviceStatus rawData
                yaml = encodePretty (defConfig & setConfCompare compare) status
            BS.putStrLn yaml
        Meter -> runDevice $ \devPtr -> do
            hideCursor
            clearScreen
            terminalWindow <- Terminal.size
            let terminalWidth = case terminalWindow of
                    Just (Terminal.Window h w) -> max 80 w
                    Nothing                    -> 80
            handle onUserInterrupt $ forM_ [0..] $ \_ -> do
                rawData <- readSaffire devPtr allMeters
                let status = updateDeviceStatus rawData
                displayMeters terminalWidth status
                threadDelay 40000
        Server -> runServer

runDevice :: (FWARef -> IO ()) -> IO ()
runDevice action = do
    success <- flip anyM nodeIds $ \nodeId ->
        handle (\(err :: DeviceError) -> pure False) $ withDevice nodeId (\devPtr -> action devPtr >> pure True)
    unless success $ putTextLn "Error communicating with the device"

displayMeters :: Int -> DeviceStatus -> IO ()
displayMeters width status = do
    setCursorPosition 0 0
    let metersScreen = Text.intercalate "\n" $ (barHeader barRange width):textVuMeters
        textVuMeters = mkFancyBar barRange width . (second ($ status ^. meters)) <$> vuMeters
    putTextLn metersScreen

barHeader :: Double -> Int -> Text
barHeader barRange width = "" +| padBothF 9 ' ' ("CHANNEL" :: Text) |+ " ▕ " +| padRightF (barWidth width - 3) ' ' (fixedF 0 (negate barRange)) |+ "0 ▏ " +| padRightF  15 ' ' ("-dB" :: Text) |+ ""

barWidth :: Int -> Int
barWidth width = width - 9 - 3 - 2 - 15

mkFancyBar :: Double -> Int -> (VUMeter, MeterValue) -> Text
mkFancyBar barRange width (m, val) = "" +| padRightF 9 ' ' (show m :: Text) |+ " ▕" +| colorVividF (if val == 0 then Red else White) (padRightF (barWidth width) ' ' (vuBar barRange (barWidth width) val)) |++| (if val == 0 then colorVividF Red ("▏◀" :: Text) else colorF White ("▏ " :: Text)) +| padLeftF 10 ' ' (fixedF 2 val) |+ ""

vuBar :: Double -> Int -> MeterValue -> Text
vuBar barRange width val | isInfinite val = ""
vuBar barRange width val = mconcat $ replicate whole "█" ++ [extra]
    where
    (whole, frac) = properFraction (n * (fromIntegral width) / barRange)
    n = max 0 $ barRange + val
    extra = extraChars ^. ix (truncate (frac * 8.0))
    extraChars =
        [ ""
        , "▏"
        , "▎"
        , "▍"
        , "▌"
        , "▋"
        , "▊"
        , "▉"
        , "█"
        ]


onUserInterrupt UserInterrupt = do
    showCursor
    putTextLn "\n"
onUserInterrupt e = throw e

---
