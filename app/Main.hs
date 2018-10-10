-- {-# INCLUDE <FWAUserLib/FWAUserLib.h> #-}

module Main where

import Universum

import Data.Default (def)

import SaffireLE.RawControl(RawControl(..))
import SaffireLE.Device
import SaffireLE.Mixer
import Fmt
import Data.Yaml.Pretty
import Control.Exception (handle)

main :: IO ()
main = do
    putTextLn "Opening device"
    forM_ nodeIds $ \nodeId ->
        handle (\(err :: DeviceError) -> putTextLn $ show err) $
            withDevice nodeId $ \devPtr -> do
                guid <- getGUID devPtr
                putTextLn $ "GUID: " +| hexF guid |+ ""
                -- readSaffire devPtr [MeteringIn1, MeteringOut1] >>= putTextLn . show
                rawData <- readSaffire devPtr allControls
                let mixer' = updateMixerState def rawData
                putStrLn  $ encodePretty (defConfig & setConfCompare compare) mixer'
                -- writeSaffire devPtr [(Pc1ToOut3, 10000)] >>= putTextLn . show
                -- readSaffire' devPtr
                pass
    putTextLn $ "Finished"
