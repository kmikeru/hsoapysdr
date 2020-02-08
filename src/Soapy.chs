{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

#include <SoapySDR/Types.h>
#include <SoapySDR/Device.h>

module Soapy where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

-- types start
{#enum define SoapySDRDirection {SOAPY_SDR_TX as DirectionTX, SOAPY_SDR_RX as DirectionRX} deriving (Eq,Show) #}
{#enum define SoapySDRChannel {0 as Channel0} deriving (Eq,Show) #}

{#pointer *SoapySDRDevice as SoapySDRDevice#}
{#pointer *SoapySDRStream as SoapySDRStream#}
{#pointer *SoapySDRKwargs as SoapySDRKwargs newtype#}

{- data SoapySDRFormat = 
    SoapySDRFormat_CF64 "CF64" -- Complex 64-bit floats (complex double)
    | SoapySDRFormat_CF32 "CF32" -- Complex 32-bit floats (complex float)
    | SoapySDRFormat_CS32 "CS32" -- Complex signed 32-bit integers (complex int32)
    | SoapySDRFormat_CU32 "CU32" -- Complex unsigned 32-bit integers (complex uint32)
    | SoapySDRFormat_CS16 "CS16" -- Complex signed 16-bit integers (complex int16)
    | SoapySDRFormat_CU16 "CU16" -- Complex unsigned 16-bit integers (complex uint16)
    | SoapySDRFormat_CS12 "CS12" -- Complex signed 12-bit integers (3 bytes)
    | SoapySDRFormat_CU12 "CU12" -- Complex unsigned 12-bit integers (3 bytes)
    | SoapySDRFormat_CS8 "CS8" -- Complex signed 8-bit integers (complex int8)
    | SoapySDRFormat_CU8 "CU8" -- Complex unsigned 8-bit integers (complex uint8)
    | SoapySDRFormat_CS4 "CS4" -- Complex signed 4-bit integers (1 byte)
    | SoapySDRFormat_CU4 "CU4" -- Complex unsigned 4-bit integers (1 byte)
    | SoapySDRFormat_F64 "F64" -- Real 64-bit floats (double)
    | SoapySDRFormat_F32 "F32" -- Real 32-bit floats (float)
    | SoapySDRFormat_S32 "S32" -- Real signed 32-bit integers (int32)
    | SoapySDRFormat_U32 "U32" -- Real unsigned 32-bit integers (uint32)
    | SoapySDRFormat_S16 "S16" -- Real signed 16-bit integers (int16)
    | SoapySDRFormat_U16 "U16" -- Real unsigned 16-bit integers (uint16)
    | SoapySDRFormat_S8 "S8" -- Real signed 8-bit integers (int8)
    | SoapySDRFormat_U8 "U8" -- Real unsigned 8-bit integers (uint8) -}
-- types end

{#fun unsafe SoapySDRDevice_setSampleRate as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_setFrequency as ^
   {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble', `SoapySDRKwargs'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_setGain as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_lastError as ^
   {} -> `CString' #}

{#fun unsafe SoapySDRDevice_makeStrArgs as ^
    {`String'} -> `SoapySDRDevice'#}

{#fun unsafe SoapySDRDevice_unmake as ^
    {`SoapySDRDevice'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_listGains as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CULong' peek*} -> `CString' peek*#}

{#fun unsafe SoapySDRDevice_listSampleRates as soapySDRDeviceListSampleRates'
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CULong' peek*} -> `Ptr CDouble' id#}

soapySDRDeviceListSampleRates dev dir chan = do
    y <- soapySDRDeviceListSampleRates' dev dir chan
    let len = fromIntegral (snd y)
    return (peekArray len (fst y))

{#fun unsafe SoapySDRDevice_getSampleRate as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel'} -> `CDouble'#}

-- foreign import ccall unsafe "SoapySDRDevice_getGainRange"
--    soapySDRDeviceGetGainRange :: SoapySDRDevice -> CInt -> CInt -> IO (SoapySDRRange)
-- SoapySDRRange SoapySDRDevice_getGainRange(const SoapySDRDevice *device, const int direction, const size_t channel); -- not a pointer

-- Stream API
{#fun unsafe SoapySDRDevice_setupStream as ^
    {`SoapySDRDevice' , alloca- `SoapySDRStream' peek*, `SoapySDRDirection', `String', with* `CULong', `CInt', `SoapySDRKwargs'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_activateStream as ^
    {`SoapySDRDevice', `SoapySDRStream', `CInt', fromIntegral `CLLong', `CULong'} -> `CInt'#}

foreign import ccall unsafe "SoapySDRDevice_readStream"
    readStream :: SoapySDRDevice -> SoapySDRStream -> Ptr (Ptr CFloat) -> CInt -> Ptr CInt -> Ptr CLLong -> CLong -> IO (CInt)

foreign import ccall unsafe "SoapySDRDevice_writeStream"
    writeStream :: SoapySDRDevice -> SoapySDRStream -> Ptr (Ptr CFloat) -> CULong -> Ptr CInt -> CLLong -> CLong -> IO (CInt)

{#fun unsafe SoapySDRDevice_deactivateStream as ^
    {`SoapySDRDevice', `SoapySDRStream', `CInt', fromIntegral `CLLong'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_closeStream as ^
    {`SoapySDRDevice', `SoapySDRStream'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_getNativeStreamFormat as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CDouble' peek*} -> `String'#}

{#fun unsafe SoapySDRDevice_getStreamFormats as soapySDRDeviceGetStreamFormats'
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CULong' peek*} -> `Ptr CString' id#}

soapySDRDeviceGetStreamFormats dev dir chan = do
    y <-soapySDRDeviceGetStreamFormats' dev dir chan
    let len = fromIntegral (snd y)
    strPtrs <- peekArray len (fst y)
    mapM peekCString strPtrs

{#fun unsafe SoapySDRDevice_getStreamMTU as ^
    {`SoapySDRDevice', `SoapySDRStream'} -> `CULong'#}

foreign import ccall unsafe "SoapySDRDevice_readStreamStatus"
    soapySDRDeviceReadStreamStatus :: SoapySDRDevice -> SoapySDRStream -> Ptr CULong -> Ptr CInt -> Ptr CLLong -> CLong -> CInt