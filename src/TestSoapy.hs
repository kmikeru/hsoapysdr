{-# LANGUAGE OverloadedStrings #-}

module TestSoapy where

import Soapy
import System.IO
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Utils
import Pipes
import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Control.Monad (forever, replicateM_)
import Data.Complex
import Data.List.Split
import Numeric.FFTW
import GHC.Float
import GHC.Word
import Data.Coerce (coerce)

sdrStream dev = do
    (output, input) <- spawn unbounded
    v <- consumeStream dev
    atomically (send output v)

receiveTest = do
    dev <- soapySDRDeviceMakeStrArgs("driver=rtlsdr")
    r1 <- soapySDRDeviceSetSampleRate dev DirectionRX Channel0 0.1e6
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionRX Channel0
    print ("samplerate:" ++ show(sr))
    a<-soapySDRDeviceSetGain dev DirectionRX Channel0 60.0
    let freq = 433.9e6
    r2 <-soapySDRDeviceSetFrequency dev DirectionRX Channel0 freq (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3
    let num_samples = 131072
    array <- allocaArray (num_samples * 2) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        timeNs <- malloc :: IO (Ptr CLLong)
        fh <- openBinaryFile "test.wav" WriteMode        
        forever $ do
            with buf $ \bufptr -> do
                elementsRead <- readStream dev stream bufptr (fromIntegral num_samples :: CInt) flags timeNs 1000000
                print elementsRead
                hPutBuf fh buf (num_samples * 2 * 4)
    print "done"

data DeviceWithStream = DeviceWithStream SoapySDRDevice SoapySDRStream

setupStream :: IO (DeviceWithStream)
setupStream = do
    dev <- soapySDRDeviceMakeStrArgs("driver=rtlsdr")
    r1 <- soapySDRDeviceSetSampleRate dev DirectionRX Channel0 1280000
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionRX Channel0
    print ("samplerate:" ++ show(sr))
    a<-soapySDRDeviceSetGain dev DirectionRX Channel0 35.0
    let freq = 105.7e6
    r2 <-soapySDRDeviceSetFrequency dev DirectionRX Channel0 freq (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3
    let devS = DeviceWithStream dev stream
    return devS

consumeStream :: DeviceWithStream -> IO [CFloat]
consumeStream (DeviceWithStream dev stream) = do
    let num_samples = 1024
    allocaArray (num_samples * 2) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        timeNs <- malloc :: IO (Ptr CLLong)
        with buf $ \bufptr -> do
            elementsRead <- readStream dev stream bufptr (fromIntegral num_samples :: CInt) flags timeNs 1000000
            free flags
            free timeNs
            peekArray (minimum(elementsRead, (num_samples * 2))) buf

toComplex :: [CFloat] -> [Complex CDouble]
toComplex floats =
    map (\i -> CDouble (i!!0) :+ CDouble (i!!1)) (chunksOf 2 doubles)
    where doubles = map (\i -> float2Double (coerce i)) floats

toComplexf :: [CFloat] -> [Complex Float]
toComplexf cfloats =
    map (\i -> (i!!0) :+ (i!!1)) (chunksOf 2 floats)
    where floats = map coerce cfloats

fftsetup = do
    inA  <- fftwAllocComplex 1024
    outA <- fftwAllocComplex 1024
    plan <- planDFT1d 1024 inA outA Forward fftwEstimate
    return (inA, outA, plan)

presetfft inA outA plan samples = do
    pokeArray inA samples
    execute plan
    res <- peekArray 1024 outA
    return res

fft :: [Complex CDouble] -> IO [Complex CDouble]
fft samples = do
    let n = length(samples)
    inA  <- fftwAllocComplex (fromIntegral n)
    outA <- fftwAllocComplex (fromIntegral n)
    plan <- planDFT1d n inA outA Forward fftwEstimate

    pokeArray inA samples
    execute plan
    res <- peekArray n outA

    fftwFree inA -- this seems to not work and results in leak
    fftwFree outA -- this seems to not work and results in leak
    return(res)

transmitTest = do
    dev <- soapySDRDeviceMakeStrArgs("driver=hackrf")
    r1 <- soapySDRDeviceSetSampleRate dev DirectionTX Channel0 0.1e6
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionTX Channel0
    print ("samplerate:" ++ show(sr))
    let freq = 433.9e6
    r2 <-soapySDRDeviceSetFrequency dev DirectionTX Channel0 freq (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionTX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3
    let num_samples = 131072
    array <- allocaArray (num_samples * 2) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        pokeArray buf [0..10000] -- just some noise
        forever $ do
            with buf $ \bufptr -> do
                elementsW <- writeStream dev stream bufptr (fromIntegral num_samples :: CULong) flags 0 1000000
                print elementsW
    print "done"

queryDevice = do
    dev <- soapySDRDeviceMakeStrArgs("driver=rtlsdr")
    formats <- soapySDRDeviceGetStreamFormats dev DirectionRX Channel0
    putStrLn ("stream formats:" ++ show(formats))
    native <- soapySDRDeviceGetNativeStreamFormat dev DirectionRX Channel0
    putStrLn ("native stream format:" ++ show(native))
    devStream <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    mtu <- soapySDRDeviceGetStreamMTU dev (snd devStream)
    putStrLn ("stream MTU:" ++ show(mtu))
    closeResult <- soapySDRDeviceCloseStream dev (snd devStream)
    putStrLn ("close result:" ++ show(closeResult))
    unmakeResult <- soapySDRDeviceUnmake dev
    putStrLn ("unmake result:" ++ show(unmakeResult))
