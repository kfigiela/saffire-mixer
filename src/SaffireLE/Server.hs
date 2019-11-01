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
import           Network.WebSockets             as WS
import           SaffireLE.Device               (DeviceError, FWARef, nodeIds, readSaffire, withDevice, writeSaffire)
import           SaffireLE.Mixer.Raw            (hardwarizeMixerState)
import qualified SaffireLE.Mixer.Raw            as M
import qualified SaffireLE.Mixer.Raw.HiRes      as HM
import           SaffireLE.Mixer.Raw.LowRes     (MatrixMixer)
import qualified SaffireLE.Mixer.Raw.LowRes     as MM
import qualified SaffireLE.Mixer.Stereo         as SM
import qualified SaffireLE.Mixer.Stereo.HiRes   as SHM
import qualified SaffireLE.Mixer.Stereo.LowRes  as SLM
import           SaffireLE.Status               (DeviceStatus, allMeters, updateDeviceStatus)
import           System.CPUTime
import           System.Directory               (createDirectoryIfMissing, getAppUserDataDirectory)
import           Text.Printf


data SaffireCmd =
    Meter
    | UpdateState SM.MixerState
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

data SaffireStatus =
    Meters DeviceStatus
    | CurrentState SM.MixerState
    deriving stock (Generic)
    deriving anyclass (ToJSON)

runServer :: IO ()
runServer = do
    deviceCommandChan <- atomically newTChan
    statusChan <- atomically newBroadcastTChan
    connectedClientCountVar <- atomically $ newTVar 0

    -- Maintain stateVar
    (readState, mixerUpdateChan) <- do
        stateFile <- statePath
        initialState <- readState stateFile
        stateVar <- atomically $ newTVar initialState
        mixerUpdateChan <- atomically newTChan
        forkIO $ forever $ do
            newState <- atomically $ do
                newState <- readTChan mixerUpdateChan
                writeTVar stateVar newState
                writeTChan deviceCommandChan $ UpdateState newState
                writeTChan statusChan  $ CurrentState newState
                pure newState
            writeState stateFile newState
        pure (readTVar stateVar, mixerUpdateChan)

    -- Request meters update when clients are connected
    forkIO $ forever $ do
        atomically $ do
            connectedClientCount <- readTVar connectedClientCountVar
            when (connectedClientCount > 0) $ writeTChan deviceCommandChan Meter
        threadDelay 40_000

    runSaffire deviceCommandChan statusChan connectedClientCountVar readState

    Warp.run 3000 (app mixerUpdateChan statusChan connectedClientCountVar readState)

runSaffire :: TChan SaffireCmd -> TChan SaffireStatus -> TVar Int -> STM SM.MixerState -> IO ()
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
        stereoMixerState <- atomically $ readState
        void $ writeSaffire devPtr $ hardwarizeMixerState $ SM.toRaw stereoMixerState

readState :: FilePath -> IO SM.MixerState
readState file = do
    state <- Y.decodeFileEither file
    case state of
        Right state -> do
            putTextLn $ "Loaded state from "+| file |+""
            pure state
        Left err -> do
            putTextLn $ "Could not load state from file, using defaults"
            pure def


writeState :: FilePath -> SM.MixerState -> IO ()
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
processCommand statusChan devPtr (UpdateState state) = do
    void $ writeSaffire devPtr $ hardwarizeMixerState $ SM.toRaw state

broadcastState :: TChan SaffireStatus -> SM.MixerState -> IO ()
broadcastState statusChan state = atomically $ writeTChan statusChan (CurrentState state)

emptyTChan :: TChan a -> STM ()
emptyTChan tchan = whileJust_ (tryReadTChan tchan) (const $ pure ())

app :: TChan SM.MixerState -> TChan SaffireStatus -> TVar Int -> STM SM.MixerState -> Application
app cmdChan statusChan connectedClientCountVar readState = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: WS.ServerApp
    wsApp pendingConn = bracket (atomically $ modifyTVar connectedClientCountVar succ) (\_ -> atomically $ modifyTVar connectedClientCountVar pred) $ \_ -> do
        conn <- acceptRequest pendingConn
        atomically readState >>= sendStatus conn . CurrentState
        void $ forkIO $ do
            localStatusChan <- atomically $ dupTChan statusChan
            forever $ atomically (readTChan localStatusChan) >>= sendStatus conn
        forever $ do
            message :: Either String SM.MixerState <- A.eitherDecode . fromDataMessage <$> receiveDataMessage conn
            print message
            case message of
                Right state -> atomically $ writeTChan cmdChan state
                Left err    -> print err

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
    sendStatus :: WS.Connection -> SaffireStatus -> IO ()
    sendStatus conn status = sendTextData conn $ A.encode $ status
