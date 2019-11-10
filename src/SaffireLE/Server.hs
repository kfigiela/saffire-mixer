{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
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
import           Network.WebSockets             as WS
import           SaffireLE.Device               (DeviceError, FWARef, nodeIds, readSaffire, withDevice, writeSaffire)
import qualified SaffireLE.Mixer                as M
import           SaffireLE.Mixer.Raw            (hardwarizeMixerState)
import qualified SaffireLE.Mixer.Raw.HiRes      as HM
import           SaffireLE.Mixer.Raw.LowRes     (MatrixMixer)
import qualified SaffireLE.Mixer.Raw.LowRes     as MM
import qualified SaffireLE.Mixer.Stereo         as SM
import qualified SaffireLE.Mixer.Stereo.HiRes   as SHM
import qualified SaffireLE.Mixer.Stereo.LowRes  as SLM
import           SaffireLE.RawControl           (RawControl (SaveSettings))
import           SaffireLE.Status               (DeviceStatus, allMeters, updateDeviceStatus)
import           System.CPUTime
import           System.Directory               (createDirectoryIfMissing, getAppUserDataDirectory)
import qualified System.Log.FastLogger          as L
import qualified System.Log.FastLogger.Date     as LD
import           Text.Printf

data DeviceCmd =
    DeviceMeter
    | DeviceUpdateState SM.MixerState SM.MixerState
    | DevicePersistState
    deriving stock (Generic, Show)

data UICmd =
    UpdateState { _state :: SM.MixerState }
    | PersistState
    deriving stock (Generic, Show)
    deriving (ToJSON, FromJSON) via (StripLensPrefix UICmd)

data SaffireStatus =
    Meters DeviceStatus
    | CurrentState SM.MixerState
    deriving stock (Generic)
    deriving anyclass (ToJSON)

type Logger = Text -> IO ()

runServer :: Int -> IO ()
runServer port = do
    timeCache <- LD.newTimeCache LD.simpleTimeFormat'
    L.withTimedFastLogger timeCache (L.LogStdout L.defaultBufSize) $ \logger -> do
        let
            log :: Logger
            log msg = logger (\time -> L.toLogStr (decodeUtf8 @Text time) <> ": " <> L.toLogStr msg <> "\n")
        log "Starting server"

        deviceCommandChan <- atomically newTChan
        statusChan <- atomically newBroadcastTChan
        connectedClientCountVar <- atomically $ newTVar 0

        -- Maintain stateVar
        (readState, mixerUpdateChan) <- do
            stateFile <- statePath
            initialState <- restorePersistedState log stateFile
            stateVar <- atomically $ newTVar initialState
            mixerUpdateChan <- atomically newTChan
            forkIO $ forever $ do
                newState <- atomically $ do
                    oldState <- readTVar stateVar
                    newState <- readTChan mixerUpdateChan
                    writeTVar stateVar newState
                    writeTChan deviceCommandChan $ DeviceUpdateState oldState newState
                    writeTChan statusChan $ CurrentState newState
                    pure newState
                writePersistentState stateFile newState
                log "Updated state"
            pure (readTVar stateVar, mixerUpdateChan)

        -- Request meters update when clients are connected
        forkIO $ forever $ do
            areClientsConnected <- atomically $ do
                connectedClientCount <- readTVar connectedClientCountVar
                let areClientsConnected = connectedClientCount > 0
                when areClientsConnected $ writeTChan deviceCommandChan DeviceMeter
                pure areClientsConnected
            threadDelay $ if areClientsConnected then 50_000 else 1_000_000
        let persistState = writeTChan deviceCommandChan DevicePersistState
        runSaffire log deviceCommandChan statusChan connectedClientCountVar readState

        Warp.run port $ app log mixerUpdateChan statusChan connectedClientCountVar readState persistState

runSaffire :: Logger -> TChan DeviceCmd -> TChan SaffireStatus -> TVar Int -> STM SM.MixerState -> IO ()
runSaffire log deviceCommandChan statusChan connectedClientCountVar readState = void $ forkIO $ do
    forever $ void $ do
        log "Trying to connect to the device"
        flip anyM nodeIds $ \nodeId -> do
            handle (\(err :: DeviceError) -> pure False) $ withDevice nodeId $ \devPtr -> do
                log $ "Connected to device at "+|| nodeId ||+""
                atomically $ flushTChan deviceCommandChan
                writeCurrentState devPtr
                forever $ atomically (readTChan deviceCommandChan) >>= processCommand log statusChan devPtr
        atomically $ writeTChan statusChan (Meters def)
        threadDelay 1_000_000
    where
    writeCurrentState devPtr = do
        stereoMixerState <- atomically readState
        void $ writeSaffire devPtr $ hardwarizeMixerState $ SM.toRaw stereoMixerState

restorePersistedState :: Logger -> FilePath -> IO SM.MixerState
restorePersistedState log file = do
    state <- M.toStereo <<$>> Y.decodeFileEither file
    case state of
        Right state -> do
            log $ "Loaded state from "+| file |+""
            pure state
        Left err -> do
            log $ "Could not load state from file, using defaults"
            pure def


writePersistentState :: FilePath -> SM.MixerState -> IO ()
writePersistentState file state = do
    let yaml = Y.encodePretty (Y.defConfig & Y.setConfCompare compare) $ M.Stereo state
    appDir <- getAppUserDataDirectory "saffire-mixer"
    createDirectoryIfMissing True appDir
    void $ BS.writeFile file yaml

statePath :: IO FilePath
statePath = do
    appDir <- getAppUserDataDirectory "saffire-mixer"
    pure $ appDir <> "/state.yaml"

processCommand :: Logger -> TChan SaffireStatus -> FWARef -> DeviceCmd -> IO ()
processCommand log statusChan devPtr DeviceMeter = do
    rawMeters <- readSaffire devPtr allMeters
    let meters = updateDeviceStatus rawMeters
    atomically $ writeTChan statusChan (Meters meters)
processCommand log statusChan devPtr (DeviceUpdateState oldState newState) = do
    let newControls = hardwarizeMixerState $ SM.toRaw newState
        oldControls = hardwarizeMixerState $ SM.toRaw oldState
        rejectEqual a b = if a == b then Nothing else Just a
        changedControls = catMaybes $ zipWith rejectEqual newControls oldControls
    writeSaffire devPtr changedControls
    log $ "Mixer state updated, updated "+|| length changedControls ||+" controls"
processCommand log statusChan devPtr DevicePersistState = do
    writeSaffire devPtr [(SaveSettings, 1)]
    log "Current settings persisted in device memory"

broadcastState :: TChan SaffireStatus -> SM.MixerState -> IO ()
broadcastState statusChan state = atomically $ writeTChan statusChan (CurrentState state)

flushTChan :: TChan a -> STM ()
flushTChan tchan = whileJust_ (tryReadTChan tchan) (const $ pure ())

app :: Logger -> TChan SM.MixerState -> TChan SaffireStatus -> TVar Int -> STM SM.MixerState -> STM () -> Application
app log cmdChan statusChan connectedClientCountVar readState persistState = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = bracket (atomically $ modifyTVar connectedClientCountVar succ) (\_ -> atomically $ modifyTVar connectedClientCountVar pred) $ \_ -> do
        conn <- acceptRequest pendingConn
        log "New client connected"
        atomically readState >>= sendStatus conn . CurrentState
        log "Sent current state to client"
        void $ forkIO $ do
            localStatusChan <- atomically $ dupTChan statusChan
            forever $ atomically (readTChan localStatusChan) >>= sendStatus conn
        handle (\(e :: ConnectionException) -> log $ "Connection error: "+|| e ||+ "") $
            forever $ do
                message :: Either String UICmd <- A.eitherDecode <$> receiveData conn
                case message of
                    Right (UpdateState state) -> atomically $ writeTChan cmdChan state
                    Right PersistState        -> atomically persistState
                    Left err                  -> log $ "Error decoding request from client: " <> toText err
        log "Client finished"

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
    sendStatus :: WS.Connection -> SaffireStatus -> IO ()
    sendStatus conn status = sendTextData conn $ A.encode $ status
