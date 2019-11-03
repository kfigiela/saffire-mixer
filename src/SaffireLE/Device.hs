{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}

module SaffireLE.Device
    ( readSaffire
    , writeSaffire
    , nodeIds
    , withDevice
    , getGUID
    , FWNodeId (..)
    , FWARef
    , DeviceError (..)
    ) where

import           Universum

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Exception    (throw)
import           Data.List.Split      (chunksOf)
import           Data.Memory.Endian   (BE, fromBE, toBE)
import qualified Data.Text            as Text
import           Fmt
import           Foreign              (alloca, allocaBytes, castPtr, peek, peekArray, plusPtr, poke, pokeArray)
import           Foreign.C.Types      (CInt (..), CUInt (..), CULong (..))
import           SaffireLE.RawControl (RawControl (..), RawControlValue, fromRawControlEnum, toRawControlEnum)

data FWADev
type FWARef = Ptr FWADev
type OSStatus = CInt

type CmdLength = CUInt
type CmdBuf = SaffireLEQuery
type ResponseBuf = SaffireLEResponse
type ResponseSize = CUInt

-- C library FFI

foreign import ccall "FWAOpen" fwaOpen' :: CUInt -> Ptr FWARef -> IO OSStatus
foreign import ccall "FWAClose" fwaClose :: FWARef -> IO OSStatus
foreign import ccall "FWAGetGUID" fwaGetGUID :: FWARef -> Ptr CULong -> IO OSStatus
foreign import ccall "FWAExecuteAVC" fwaExecuteAVC' :: FWARef -> Ptr CmdBuf -> CmdLength -> Ptr ResponseBuf -> Ptr ResponseSize -> IO OSStatus

fwaOpen :: FWNodeId -> Ptr FWARef -> IO ()
fwaOpen nid@(FWNodeId nodeId) devPtr = assertSuccess (Open nid) $ fwaOpen' (CUInt nodeId) devPtr

fwaExecuteAVC :: FWARef -> Ptr CmdBuf -> CmdLength -> Ptr ResponseBuf -> Ptr ResponseSize -> IO ()
fwaExecuteAVC = assertSuccess AVCFailed ... fwaExecuteAVC'

assertSuccess :: DeviceError -> IO OSStatus -> IO ()
assertSuccess err io = do
    status <- io
    unless (status == 0) $ throw err

newtype FWNodeId = FWNodeId Word32 deriving (Show, Eq)

data DeviceError = Open FWNodeId | CloseFailed | AVCFailed | GetGUID deriving (Show, Eq)
instance Exception DeviceError

-- AVC protocol types

data AVCOp = Write | Read deriving (Show, Eq, Enum)

data SaffireLEQuery =
    SaffireLEQuery
    { _op      :: AVCOp
    , _payload :: [(RawControl, RawControlValue)]
    } deriving (Show, Eq)

data SaffireLEResponse =
    SaffireLEResponse
    { _payload :: [(RawControl, RawControlValue)]
    } deriving (Show, Eq)

-- protocol decoding

arrayToEntry :: [BE Word32] -> [Either Text (RawControl, RawControlValue)]
arrayToEntry (controlId:value:tail) =  case toRawControlEnum (fromBE controlId) of
    Just control -> (Right (control, fromBE value)):(arrayToEntry tail)
    Nothing      -> (Left $ "got garbage from device: "+| hexF (fromBE controlId) |+" = "+| hexF (fromBE value) |+""):(arrayToEntry tail)
arrayToEntry [_]                    = error "odd number of values"
arrayToEntry []                     = []

entryToRow :: (RawControl, RawControlValue) -> [BE Word32]
entryToRow (control, value) = [toBE $ fromRawControlEnum control, toBE value]

peekResponse :: Ptr SaffireLEResponse -> IO SaffireLEResponse
peekResponse ptr = do
        header :: [Word8] <- peekArray 8 (castPtr ptr)
        let [_, _, _, _, _, _, _, count] = header
        rows <- peekArray (2 * fromIntegral count) ((castPtr ptr) `plusPtr` 8)

        let payloadOrErrors = arrayToEntry rows
            payload = rights payloadOrErrors
            errors = lefts payloadOrErrors

        when (length errors > 0) $ putTextLn $ Text.intercalate "\n" errors
        pure $ SaffireLEResponse { _payload = payload}

pokeQuery :: Ptr SaffireLEQuery -> SaffireLEQuery -> IO ()
pokeQuery ptr SaffireLEQuery { _op = op, _payload = payload } =
    let header :: [Word8]
        header = [fromIntegral (fromEnum op), 0xff, 0x00, 0x00, 0x13, 0x0e, 0x03, fromIntegral (length payload)]
        payload' = mconcat $ entryToRow <$> payload
    in do
        pokeArray (castPtr ptr) header
        pokeArray (castPtr ptr `plusPtr` 8) payload'

headerSize, bytesPerControl :: Int
headerSize = 8
bytesPerControl = 8

executeSaffireAVC :: AVCOp -> FWARef -> [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
executeSaffireAVC avcOp devPtr controls = mconcat <$> chunkResults
    where
        chunks = chunksOf 32 controls
        chunkResults = mapM chunkResult chunks
        chunkResult = executeSaffireAVC' avcOp devPtr

executeSaffireAVC' :: AVCOp -> FWARef -> [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
executeSaffireAVC' avcOp devPtr controls =
    let buffSize = headerSize + bytesPerControl * length controls
    in
    allocaBytes buffSize $ \(cmdPtr :: Ptr SaffireLEQuery)  ->
    allocaBytes buffSize $ \(resultPtr :: Ptr SaffireLEResponse) ->
    alloca               $ \(resultSizePtr :: Ptr CUInt) -> do
        let cmd = (SaffireLEQuery avcOp controls)
        pokeQuery cmdPtr cmd
        poke resultSizePtr (fromIntegral buffSize)

        fwaExecuteAVC devPtr cmdPtr (fromIntegral buffSize) resultPtr resultSizePtr
        resultSize <- peek resultSizePtr
        SaffireLEResponse fields <- peekResponse resultPtr
        pure fields

---

-- | Saffire LE appears on the two possible node ids
nodeIds :: [FWNodeId]
nodeIds = FWNodeId <$> [0xffc0, 0xffc2]

withDevice :: FWNodeId -> (FWARef -> IO a) -> IO a
withDevice nodeId f = alloca $ \devPtr -> bracket (fwaOpen nodeId devPtr >> peek devPtr) fwaClose f

readSaffire :: FWARef -> [RawControl] -> IO [(RawControl, RawControlValue)]
readSaffire devPtr controls = executeSaffireAVC Read devPtr $ (, 0) <$> controls

writeSaffire :: FWARef ->  [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
writeSaffire = executeSaffireAVC Write

getGUID :: FWARef -> IO Word64
getGUID devPtr = alloca $ \guidPtr -> do
    assertSuccess GetGUID $ fwaGetGUID devPtr guidPtr
    CULong guid <- peek guidPtr
    pure guid
