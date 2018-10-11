module SaffireLE.CLI where

import Universum

import Data.Default (def)


import Fmt
import Data.Yaml (decodeFileThrow)
import Data.Yaml.Pretty
import Control.Exception (handle, AsyncException(..), throw)
import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Text as Text

import SaffireLE.Device
import SaffireLE.Mixer
import Control.Concurrent
import SaffireLE.Status
import System.Console.ANSI

data Command
  = LoadSettings { _filename :: FilePath }
  | DumpSettings { _filename :: FilePath }
  | Meter
  | FancyMeter
  deriving (Eq, Show)

loadSettings :: Parser Command
loadSettings = LoadSettings <$> argument str (metavar "FILE")

dumpSettings :: Parser Command
dumpSettings = DumpSettings <$> argument str (metavar "FILE")


cli :: Parser Command
cli = subparser
   (  command "load"  (info loadSettings (progDesc "Load settings from file to device"))
   <> command "dump"  (info dumpSettings (progDesc "Dump settings from device to file"))
   <> command "meter" (info (pure Meter) (progDesc "Read data from DSP meters"))
   <> command "fancymeter" (info (pure FancyMeter) (progDesc "Read data from DSP meters"))
   )

main :: IO ()
main = execParser opts >>= runCommands

opts :: ParserInfo Command
opts = info (cli <**> helper) idm

runCommands :: Command -> IO ()
runCommands cmd = do
    success <- flip anyM nodeIds $ \nodeId ->
        handle (\(err :: DeviceError) -> pure False) $ withDevice nodeId $ \devPtr -> do
            case cmd of
                LoadSettings file -> do
                    mixerState <- decodeFileThrow file
                    let rawData = hardwarizeMixerState mixerState
                    void $ writeSaffire devPtr rawData
                DumpSettings file -> do
                    rawData <- readSaffire devPtr allControls
                    let mixer' = updateMixerState def rawData
                        yaml = encodePretty (defConfig & setConfCompare compare) mixer'
                    void $ BS.writeFile file yaml
                Meter -> do
                    rawData <- readSaffire devPtr allMeters
                    let status = updateDeviceStatus def rawData
                        yaml = encodePretty (defConfig & setConfCompare compare) status
                    BS.putStrLn yaml
                FancyMeter -> do
                    hideCursor
                    clearScreen
                    handle onUserInterrupt $ forM_ [0..] $ \_ -> do
                        rawData <- readSaffire devPtr allMeters
                        let status = updateDeviceStatus def rawData
                        setCursorPosition 0 0
                        putTextLn $ Text.intercalate "\n" $ mkFancyBar <$> Map.toList (status ^. meters)
                        threadDelay 20090
            pure True
    unless success $ putTextLn "Error communicating with the device"



mkFancyBar :: (Meter, MeterValue) -> Text
mkFancyBar (m, val) = "<" +| padBothF 11 ' ' (m ^. name) |+ "> " +| vuBar val |+" " +| padLeftF 20 ' ' (fixedF 2 val) |+ ""

vuBar :: MeterValue -> Text
vuBar val = mconcat $ replicate n "#" ++ replicate (80-n) "-" where n = max 0 $ floor $ 80 + val

onUserInterrupt UserInterrupt = do
    showCursor
    putTextLn "\nUserInterruption"
onUserInterrupt e = throw e
