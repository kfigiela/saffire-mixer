{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module SaffireLE.Server where

import           Universum                      hiding (State)

import           Data.Default.Class             (def)

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar    (modifyTVar)
import           Control.Exception              hiding (bracket)
import           Control.Lens.TH                (makeLenses)
import           Control.Monad.Loops            (iterateM_, whileJust_)
import           Data.Aeson                     (FromJSON, ToJSON)
import qualified Data.Aeson                     as A
import           Data.Aeson.Extra               (StripLensPrefix (..))
import qualified Data.ByteString.Char8          as BS
import           Data.Default.Class             (Default, def)
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
import qualified SaffireLE.Mixer.HiResStereo    as SHM
import           SaffireLE.Mixer.Matrix         (MatrixMixer)
import qualified SaffireLE.Mixer.Matrix         as MM
import qualified SaffireLE.Mixer.Stereo         as SM
import           SaffireLE.Status
import           System.CPUTime
import           System.Directory               (createDirectoryIfMissing, getAppUserDataDirectory)
import           Text.Printf

data State =
    State
    { _mixer       :: !M.MixerState
    , _stereo      :: !SM.StereoMixer
    , _hiResStereo :: !SHM.StereoMixer
    }
    deriving stock (Generic, Show)
    deriving anyclass (Default)
    deriving (ToJSON, FromJSON) via (StripLensPrefix State)

makeLenses ''State

data SaffireCmd =
    Meter
    | SetState State
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
    | SetHighResStereoMixer { _highResStereoMix :: SHM.StereoMixer }
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

    deviceCommandChan <- atomically newTChan
    statusChan <- atomically newBroadcastTChan
    connectedClientCountVar <- atomically $ newTVar 0

    -- Maintain stateVar
    (readState, mixerCmdChan) <- do
        stateFile <- statePath
        initialState <- readState stateFile
        stateVar <- atomically $ newTVar initialState
        mixerCmdChan <- atomically newTChan
        forkIO $ forever $ do
            newState <- atomically $ do
                mixerCmd <- readTChan mixerCmdChan
                modifyTVar stateVar $ processMixerCmd mixerCmd
                newState <- readTVar stateVar
                writeTChan deviceCommandChan $ SetState newState
                writeTChan statusChan  $ CurrentState newState
                pure newState
            writeState stateFile newState
        pure (readTVar stateVar, mixerCmdChan)

    -- Request meters update when clients are connected
    forkIO $ forever $ do
        atomically $ do
            connectedClientCount <- readTVar connectedClientCountVar
            when (connectedClientCount > 0) $ writeTChan deviceCommandChan Meter
        threadDelay 40_000

    runSaffire deviceCommandChan statusChan connectedClientCountVar readState

    Warp.run 3000 (app mixerCmdChan statusChan connectedClientCountVar readState)

runSaffire :: TChan SaffireCmd -> TChan SaffireStatus -> TVar Int -> STM State -> IO ()
runSaffire deviceCommandChan statusChan connectedClientCountVar readState = void $ forkIO $ do
    forever $ void $ do
        putTextLn "Trying to connect to the device"
        flip anyM nodeIds $ \nodeId -> do
            handle (\(err :: DeviceError) -> pure False) $ withDevice nodeId $ \devPtr -> do
                putTextLn $ "Connected to device at "+|| nodeId ||+""
                atomically $ emptyTChan deviceCommandChan
                writeCurrentState devPtr
                forever $ atomically (readTChan deviceCommandChan) >>= processCommand statusChan devPtr
        threadDelay 1_000_000
    where
    writeCurrentState devPtr = do
        State matrix _ _ <- atomically $ readState
        void $ writeSaffire devPtr $ hardwarizeMixerState matrix

readState :: FilePath -> IO State
readState file = do
    state <- Y.decodeFileEither file
    case state of
        Right state -> do
            putTextLn $ "Loaded state from "+| file |+""
            pure state
        Left err -> do
            putTextLn $ "Could not load state from file, using defaults"
            pure def


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


processCommand :: TChan SaffireStatus -> FWARef -> SaffireCmd -> IO ()
processCommand statusChan devPtr Meter = do
    rawMeters <- readSaffire devPtr allMeters
    let meters = updateDeviceStatus def rawMeters
    atomically $ writeTChan statusChan (Meters meters)
processCommand statusChan devPtr (SetState (State matrix _stereo _hiResStereo)) = do
    void $ writeSaffire devPtr $ hardwarizeMixerState matrix

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
               & hiResStereo .~ SHM.toStereoMixer mix
processMixerCmd (SetHighResStereoMixer mix) mixerState =
    mixerState & mixer . M.highResMixer .~ SHM.fromStereoMixer mix
               & hiResStereo .~ mix

emptyTChan :: TChan a -> STM ()
emptyTChan tchan = whileJust_ (tryReadTChan tchan) (const $ pure ())


app :: TChan MixerCmd -> TChan SaffireStatus -> TVar Int -> STM State -> Application
app cmdChan statusChan connectedClientCountVar readState = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pendingConn = bracket (atomically $ modifyTVar connectedClientCountVar succ) (\_ -> atomically $ modifyTVar connectedClientCountVar pred) $ \_ -> do
        conn <- acceptRequest pendingConn
        atomically readState >>= sendStatus conn . CurrentState
        void $ forkIO $ do
            localStatusChan <- atomically $ dupTChan statusChan
            forever $ atomically (readTChan localStatusChan) >>= sendStatus conn
        forever $ do
            message :: Maybe MixerCmd <- A.decode . fromDataMessage <$> receiveDataMessage conn
            print message
            whenJust message $ atomically . writeTChan cmdChan

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
    sendStatus :: Connection -> SaffireStatus -> IO ()
    sendStatus conn status = sendTextData conn $ A.encode $ status
