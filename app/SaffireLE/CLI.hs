module SaffireLE.CLI where

import Universum

import Data.Default (def)

import SaffireLE.Device
import SaffireLE.Mixer
import Fmt
import Data.Yaml (decodeFileThrow)
import Data.Yaml.Pretty
import Control.Exception (handle)
import Options.Applicative
import qualified Data.ByteString as BS

data Command
  = LoadSettings { _filename :: FilePath }
  | DumpSettings { _filename :: FilePath }
  | Meter
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
   )

main :: IO ()
main = execParser opts >>= runCommands

opts :: ParserInfo Command
opts = info (cli <**> helper) idm

runCommands :: Command -> IO ()
runCommands cmd =
    forM_ nodeIds $ \nodeId ->
    handle (\(err :: DeviceError) -> putTextLn $ show err) $
        withDevice nodeId $ \devPtr -> do
            guid <- getGUID devPtr
            putTextLn $ "GUID: " +| hexF guid |+ ""
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
                    pass
                    rawData <- readSaffire devPtr allMeters
                    let mixer' = updateDeviceState def rawData
                        yaml = encodePretty (defConfig & setConfCompare compare) mixer'
                    putTextLn $ blockListF' "- " show rawData |+""
                    BS.putStrLn yaml
