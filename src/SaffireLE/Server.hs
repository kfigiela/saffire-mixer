{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module SaffireLE.Server where

import           Universum                      hiding (State)

import           Data.Default.Class             (def)

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Exception              hiding (bracket)
import           Control.Lens.TH                (makeLenses)
import           Control.Monad.Loops            (iterateM_, whileJust_)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Aeson                     as A
import           Data.Aeson.Extra               (StripLensPrefix (..))
import qualified Data.ByteString.Char8          as BS
import qualified Data.Yaml                      as Y
import qualified Data.Yaml.Pretty               as Y
import           Fmt
import           Network.HTTP.Types             (status200, status400)
import           Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp (run)
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           SaffireLE.Device
import           SaffireLE.Mixer
import qualified SaffireLE.Mixer                as M
import qualified SaffireLE.Mixer.HiRes          as HM
import           SaffireLE.Mixer.Matrix         (MatrixMixer)
import qualified SaffireLE.Mixer.Matrix         as MM
import qualified SaffireLE.Mixer.Stereo         as SM
import           SaffireLE.Status
import           System.CPUTime
import           System.Directory               (createDirectoryIfMissing, getAppUserDataDirectory)
import           Text.Printf

data State =
    State
    { _mixer  :: !M.MixerState
    , _stereo :: !SM.StereoMixer
    }
    deriving stock (Generic, Show)
    deriving (ToJSON, FromJSON) via (StripLensPrefix State)

makeLenses ''State

data SaffireCmd =
    Meter
    | GetState
    | MixerCmd { cmd :: MixerCmd }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

data MixerCmd =
    Noop
    | Mute { _output :: OutputPair, _muted :: Bool }
    | Attenuate { _output :: OutputPair, _db :: Word8 }
    | InGain { _input :: InputChannel, _gainOn :: Bool }
    | MidiThru { _midiThru :: Bool }
    | SPDIFTransparent { _spdifTransparent :: Bool }
    | SetMatrixMixer { _matrixMix :: MM.MatrixMixer }
    | SetStereoMixer { _stereoMix :: SM.StereoMixer }
    | SetHighResMixer { _highResMix :: HM.Mixer }
    deriving stock (Generic, Show)
    deriving (ToJSON, FromJSON) via (StripLensPrefix MixerCmd)

data OutputPair = Out12 | Out34 | Out56
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

outputPairOpts :: OutputPair -> Lens' MixerState OutOpts
outputPairOpts Out12 = out12Opts
outputPairOpts Out34 = out34Opts
outputPairOpts Out56 = out56Opts

data InputChannel = Ch3 | Ch4
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

inChGain :: InputChannel -> Lens' MixerState Bool
inChGain Ch3 = in3Gain
inChGain Ch4 = in4Gain

data SaffireStatus =
    Meters DeviceStatus
    | CurrentState State
    deriving stock (Generic)
    deriving anyclass (ToJSON)

runServer :: IO ()
runServer = do
    connectedClientCountVar <- atomically $ newTVar 0
    saffireCommandChan <- atomically newTChan
    saffireStatusChan <- atomically newBroadcastTChan
    forkIO $ forever $ do
        atomically $ do
            connectedClientCount <- readTVar connectedClientCountVar
            when (connectedClientCount > 0) $ writeTChan saffireCommandChan Meter
        threadDelay 40_000

    runSaffire saffireCommandChan saffireStatusChan connectedClientCountVar

    Warp.run 3000 (app saffireCommandChan saffireStatusChan connectedClientCountVar)

runSaffire :: TChan SaffireCmd -> TChan SaffireStatus -> TVar Int -> IO ()
runSaffire cmdChan statusChan connectedClientCountVar = void $ forkIO $ do
    stateFile <- statePath
    forever $ void $ do
        putTextLn "Trying to connect to the device"
        flip anyM nodeIds $ \nodeId -> do
            handle (\(err :: DeviceError) -> pure False) $ withDevice nodeId $ \devPtr -> do
                putTextLn $ "Connected to device at "+|| nodeId ||+""
                atomically $ emptyTChan cmdChan
                initialState <- readState devPtr stateFile
                flip iterateM_ initialState $ \state -> do
                    cmd <- atomically (readTChan cmdChan)
                    processCommand statusChan devPtr (writeState stateFile) state cmd
        threadDelay 1_000_000

readState :: FWARef -> FilePath -> IO State
readState devPtr file = do
    state <- Y.decodeFileEither file
    case state of
        Right state -> do
            putTextLn $ "Loaded state from "+| file |+""
            pure state
        Left err -> do
            putTextLn $ "Could not parse "+| file |+": "
            putStrLn $ Y.prettyPrintParseException err
            putTextLn $ "Reading data from the device..."
            initialMixerState <- updateMixerState def <$> readSaffire devPtr allControls
            let stereoMixer = SM.toStereoMixer $ initialMixerState ^. M.lowResMixer
                state = State initialMixerState stereoMixer
            writeState file state
            pure state

writeState :: FilePath -> State -> IO ()
writeState file state = do
    let yaml = Y.encodePretty (Y.defConfig & Y.setConfCompare compare) state
    appDir <- getAppUserDataDirectory "saffire-mixer"
    createDirectoryIfMissing True appDir
    void $ BS.writeFile file yaml

statePath :: IO FilePath
statePath = do
    appDir <- getAppUserDataDirectory "saffire-mixer"
    pure $ appDir <> "/state.yaml"


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v


processCommand :: TChan SaffireStatus -> FWARef -> (State -> IO ()) -> State -> SaffireCmd -> IO State
processCommand statusChan devPtr _ state Meter = do
    rawMeters <- readSaffire devPtr allMeters
    let meters = updateDeviceStatus def rawMeters
    atomically $ writeTChan statusChan (Meters meters)
    pure state
processCommand statusChan devPtr _ state GetState = do
    broadcastState statusChan state
    pure state
processCommand statusChan devPtr saveState state (MixerCmd mixerCmd) = do
    let
        state' = processMixerCmd mixerCmd state
        State matrix stereo = state'
        rawData = hardwarizeMixerState matrix
    void $ writeSaffire devPtr rawData
    broadcastState statusChan state'
    saveState state
    pure state'

broadcastState :: TChan SaffireStatus -> State -> IO ()
broadcastState statusChan state = atomically $ writeTChan statusChan (CurrentState state)

processMixerCmd :: MixerCmd -> State -> State
processMixerCmd (Mute out muted) mixerState =
    mixerState & mixer . outputPairOpts out . mute .~ muted
               & mixer . outputPairOpts out . mute .~ muted
               & mixer . outputPairOpts out . mute .~ muted
processMixerCmd (Attenuate out db) mixerState =
    mixerState & mixer . outputPairOpts out . attenuation .~ db
               & mixer . outputPairOpts out . attenuation .~ db
               & mixer . outputPairOpts out . attenuation .~ db
processMixerCmd (InGain channel gainOn) mixerState =
    mixerState & mixer . inChGain channel .~ gainOn
processMixerCmd (MidiThru value) mixerState =
    mixerState & mixer . M.midiThru .~ value
processMixerCmd (SPDIFTransparent value) mixerState =
    mixerState & mixer . M.spdifTransparent .~ value
processMixerCmd (SetMatrixMixer matrix) mixerState =
    mixerState & mixer . M.lowResMixer .~ matrix
               & stereo .~ SM.toStereoMixer matrix
processMixerCmd (SetStereoMixer mix) mixerState =
    mixerState & mixer . M.lowResMixer .~ SM.fromSteroMixer mix
               & stereo .~ mix
processMixerCmd (SetHighResMixer mix) mixerState =
    mixerState & mixer . M.highResMixer .~ mix


emptyTChan :: TChan a -> STM ()
emptyTChan tchan = whileJust_ (tryReadTChan tchan) (const $ pure ())


app :: TChan SaffireCmd -> TChan SaffireStatus -> TVar Int -> Application
app cmdChan statusChan connectedClientCountVar = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pendingConn = bracket (atomically $ modifyTVar connectedClientCountVar succ) (\_ -> atomically $ modifyTVar connectedClientCountVar pred) $ \_ -> do
        conn <- acceptRequest pendingConn
        void $ forkIO $ do
            localStatusChan <- atomically $ dupTChan statusChan
            atomically $ writeTChan cmdChan GetState
            forever $ do
                message <- atomically $ readTChan localStatusChan
                sendTextData conn $ A.encode message
        forever $ do
            message :: Maybe MixerCmd <- A.decode . fromDataMessage <$> receiveDataMessage conn
            print message
            whenJust message $ atomically . writeTChan cmdChan . MixerCmd

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
