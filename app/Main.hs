{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# INCLUDE <FWAUserLib/FWAUserLib.h> #-}

module Main where

import Universum

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Memory.Endian

import Lib
import Fmt

data FWADev

type FWARef = Ptr FWADev

type OSStatus = CInt

type CmdLength = CUInt

type CmdBuf = SaffireLEQuery -- array of

type ResponseBuf = SaffireLEResponse -- array of

type ResponseSize = CUInt


foreign import ccall "FWAOpen" fwaOpen :: CUInt -> Ptr FWARef -> IO OSStatus
foreign import ccall "FWAClose" fwaClose :: FWARef -> IO OSStatus
foreign import ccall "FWAGetGUID" fwaGetGUID :: FWARef -> Ptr CULong -> IO OSStatus
foreign import ccall "FWAExecuteAVC" fwaExecuteAVC :: FWARef -> Ptr CmdBuf -> CmdLength -> Ptr ResponseBuf -> Ptr ResponseSize -> IO OSStatus

data AVCOp = Write | Read  deriving (Show, Eq, Enum)

type Field = Word32
type Value = Word32

data SaffireLEQuery =
    SaffireLEQuery
    { _op :: AVCOp
    , _payload :: [(Control, Value)]
    } deriving (Show, Eq)

data SaffireLEResponse =
    SaffireLEResponse
    { _payload :: [(Control, Value)]
    } deriving (Show, Eq)


arrayToEntry :: [BE Word32] -> [(Control, Value)]
arrayToEntry (controlId:value:tail) = ((toEnum $ fromIntegral $ fromBE controlId, fromBE value)):(arrayToEntry tail)
arrayToEntry [_] = error "odd number of values"
arrayToEntry [] = []

entryToRow :: (Control, Value) -> [BE Word32]
entryToRow (control, value) = [toBE $ fromIntegral $ fromEnum control, toBE value]

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

readSaffire :: FWARef -> [Control] -> IO [(Control, Value)]
readSaffire devPtr controls =
    let buffSize = headerSize + 8 * length controls
    in
    allocaBytes buffSize $ \(cmdPtr :: Ptr SaffireLEQuery)  ->
    allocaBytes buffSize $ \(resultPtr :: Ptr SaffireLEResponse) ->
    alloca               $ \(resultSizePtr :: Ptr CUInt) -> do
        let fields = (\ctl -> (ctl, 0)) <$> controls
            cmd = (SaffireLEQuery Read fields)
        pokeQuery cmdPtr cmd
        poke resultSizePtr (fromIntegral buffSize)

        status <- fwaExecuteAVC devPtr cmdPtr (fromIntegral buffSize) resultPtr resultSizePtr
        putTextLn $ "AVC status: " +|| status ||+ ""
        resultSize <- peek resultSizePtr
        print resultSize
        SaffireLEResponse fields <- peekResponse resultPtr
        pure fields


writeSaffire :: FWARef -> [(Control, Value)] -> IO [(Control, Value)]
writeSaffire devPtr controls =
    let buffSize = headerSize + 8 * length controls
    in
    allocaBytes buffSize $ \(cmdPtr :: Ptr SaffireLEQuery)  ->
    allocaBytes buffSize $ \(resultPtr :: Ptr SaffireLEResponse) ->
    alloca               $ \(resultSizePtr :: Ptr CUInt) -> do
        let cmd = (SaffireLEQuery Write controls)
        pokeQuery cmdPtr cmd
        poke resultSizePtr (fromIntegral buffSize)

        status <- fwaExecuteAVC devPtr cmdPtr (fromIntegral buffSize) resultPtr resultSizePtr
        putTextLn $ "AVC status: " +|| status ||+ ""
        resultSize <- peek resultSizePtr
        print resultSize
        SaffireLEResponse fields <- peekResponse resultPtr
        pure fields


-- readSaffire' :: FWARef -> IO ()
-- readSaffire' devPtr =
--     allocaBytes 16 $ \(cmdPtr :: Ptr CChar)  ->
--     allocaBytes 256 $ \(resultPtr :: Ptr CChar) ->
--     alloca           $ \resultSizePtr -> do
--         let request :: [CChar] = [ 1,  0xff, 0x00, 0x00, 0x13, 0x0e, 0x03, 1
--                                  , 0     ,0 ,    0 ,   0x5a,    0   , 0    , 0 ,  0]
--         pokeArray cmdPtr request
--         poke resultSizePtr 256
--         status <- fwaExecuteAVC devPtr cmdPtr 16 resultPtr resultSizePtr
--         putTextLn $ "AVC status: " +|| hexF status ||+ ""
--         resultSize <- peek resultSizePtr
--         print resultSize
--         result :: [CChar] <- peekArray 16 resultPtr
--         putTextLn $ "Result: " +| listF' hexF result |+ ""

--         pass


newtype FWNodeId = FWNodeId Word32

nodeId :: FWNodeId
nodeId = FWNodeId 0xffc2

withDevice :: FWNodeId -> (FWARef -> IO ()) -> IO Bool
withDevice (FWNodeId nodeId) f = alloca $ \devPtr -> do
    status <- fwaOpen (CUInt nodeId) devPtr
    devPtr' <- peek devPtr
    if status == 0
        then do
            f devPtr'
            _ <- fwaClose devPtr'
            pure True
        else do
            putTextLn "Failed to open device"
            pure False

getGUID :: FWARef -> IO Word64
getGUID devPtr = alloca $ \guidPtr -> do
    status <- fwaGetGUID devPtr guidPtr
    putTextLn $ "GUID status: " +|| hexF status ||+ ""

    (CULong guid) <- peek guidPtr
    pure guid




main :: IO ()
main = do
    putTextLn "Opening device"
    _ <- withDevice nodeId $ \devPtr -> do
        guid <- getGUID devPtr
        putTextLn $ "GUID: " +| hexF guid |+ ""
        -- readSaffire devPtr [MeteringIn1, MeteringOut1] >>= putTextLn . show
        writeSaffire devPtr [(Pc1ToOut3, 10000)] >>= putTextLn . show
        -- readSaffire' devPtr
        pass
    putTextLn $ "Finished"
