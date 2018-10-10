{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SaffireLE.Device where

import Universum

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Memory.Endian
import Control.Exception (throw)
import Data.List.Split (chunksOf)
import Fmt

import SaffireLE.RawControl (RawControlId, RawControlValue, RawControl(..), BitField(..), toRawControlEnum, fromRawControlEnum)

data FWADev
type FWARef = Ptr FWADev
type OSStatus = CInt

type CmdLength = CUInt
type CmdBuf = SaffireLEQuery
type ResponseBuf = SaffireLEResponse
type ResponseSize = CUInt


foreign import ccall "FWAOpen" fwaOpen' :: CUInt -> Ptr FWARef -> IO OSStatus
foreign import ccall "FWAClose" fwaClose :: FWARef -> IO OSStatus
foreign import ccall "FWAGetGUID" fwaGetGUID :: FWARef -> Ptr CULong -> IO OSStatus
foreign import ccall "FWAExecuteAVC" fwaExecuteAVC :: FWARef -> Ptr CmdBuf -> CmdLength -> Ptr ResponseBuf -> Ptr ResponseSize -> IO OSStatus


fwaOpen :: FWNodeId -> Ptr FWARef -> IO ()
fwaOpen nid@(FWNodeId nodeId) devPtr = assertSuccess (Open nid) $ fwaOpen' (CUInt nodeId) devPtr


data AVCOp = Write | Read deriving (Show, Eq, Enum)


type Value = Word32

data SaffireLEQuery =
    SaffireLEQuery
    { _op :: AVCOp
    , _payload :: [(RawControl, RawControlValue)]
    } deriving (Show, Eq)

data SaffireLEResponse =
    SaffireLEResponse
    { _payload :: [(RawControl, RawControlValue)]
    } deriving (Show, Eq)

data DeviceError = Open FWNodeId | CloseFailed | AVCFailed deriving (Show, Eq)

instance Exception DeviceError

arrayToEntry :: [BE Word32] -> [(RawControl, RawControlValue)]
arrayToEntry (controlId:value:tail) = ((toRawControlEnum $ fromBE controlId, fromBE value)):(arrayToEntry tail)
arrayToEntry [_] = error "odd number of values"
arrayToEntry [] = []

entryToRow :: (RawControl, RawControlValue) -> [BE Word32]
entryToRow (control, value) = [toBE $ fromRawControlEnum control, toBE value]

peekResponse :: Ptr SaffireLEResponse -> IO SaffireLEResponse
peekResponse ptr = do
        header :: [Word8] <- peekArray 8 (castPtr ptr)
        let [op, _, _, _, _, _, _, count] = header
        rows <- peekArray (2 * fromIntegral count) ((castPtr ptr) `plusPtr` 8)

        let payload = arrayToEntry rows
        pure $ SaffireLEResponse { _payload = payload}

pokeQuery :: Ptr SaffireLEQuery -> SaffireLEQuery -> IO ()
pokeQuery ptr SaffireLEQuery { _op = op, _payload = payload } =
    let header :: [Word8]
        header = [fromIntegral (fromEnum op), 0xff, 0x00, 0x00, 0x13, 0x0e, 0x03, fromIntegral (length payload)]
        payload' = mconcat $ entryToRow <$> payload
    in do
        putTextLn $ "Header: " +| listF' hexF header |+ ""
        putTextLn $ "Payload: " +| listF' hexF (unBE <$> payload') |+ ""
        pokeArray (castPtr ptr) header
        pokeArray (castPtr ptr `plusPtr` 8) payload'

headerSize :: Int
headerSize = 8

readSaffire :: FWARef -> [RawControl] -> IO [(RawControl, RawControlValue)]
readSaffire devPtr controls = writeSaffire' Read devPtr ((\ctl -> (ctl, 0)) <$> controls)

writeSaffire :: FWARef ->  [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
writeSaffire = writeSaffire' Write



writeSaffire' :: AVCOp -> FWARef -> [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
writeSaffire' avcOp devPtr controls = mconcat <$> chunkResults
    where
        chunks = chunksOf 32 controls
        chunkResults = mapM chunkResult chunks
        chunkResult controls = writeSaffire'' avcOp devPtr controls


writeSaffire'' :: AVCOp -> FWARef -> [(RawControl, RawControlValue)] -> IO [(RawControl, RawControlValue)]
writeSaffire'' avcOp devPtr controls =
    let buffSize = headerSize + 8 * length controls
    in
    allocaBytes buffSize $ \(cmdPtr :: Ptr SaffireLEQuery)  ->
    allocaBytes buffSize $ \(resultPtr :: Ptr SaffireLEResponse) ->
    alloca               $ \(resultSizePtr :: Ptr CUInt) -> do
        let cmd = (SaffireLEQuery avcOp controls)
        pokeQuery cmdPtr cmd
        poke resultSizePtr (fromIntegral buffSize)

        assertSuccess AVCFailed $ fwaExecuteAVC devPtr cmdPtr (fromIntegral buffSize) resultPtr resultSizePtr
        resultSize <- peek resultSizePtr
        print resultSize
        SaffireLEResponse fields <- peekResponse resultPtr
        pure fields

newtype FWNodeId = FWNodeId Word32 deriving (Show, Eq)

nodeIds :: [FWNodeId]
nodeIds = FWNodeId <$> [0xffc0, 0xffc2]

withDevice :: FWNodeId -> (FWARef -> IO ()) -> IO ()
withDevice nodeId f = alloca $ \devPtr -> bracket (fwaOpen nodeId devPtr >> peek devPtr) fwaClose f

assertSuccess :: DeviceError -> IO OSStatus -> IO ()
assertSuccess err io = do
    status <- io
    unless (status == 0) $ throw err

getGUID :: FWARef -> IO Word64
getGUID devPtr = alloca $ \guidPtr -> do
    status <- fwaGetGUID devPtr guidPtr
    putTextLn $ "GUID status: " +|| hexF status ||+ ""

    (CULong guid) <- peek guidPtr
    pure guid
