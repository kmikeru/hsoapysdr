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

-- types end

{#fun unsafe SoapySDRDevice_setSampleRate as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_setFrequency as ^
   {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble', `SoapySDRKwargs'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_setGain as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', `CDouble'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_setupStream as ^
    {`SoapySDRDevice' , alloca- `SoapySDRStream' peek*, `SoapySDRDirection', `String', with* `CULong', `CInt', `SoapySDRKwargs'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_activateStream as ^
    {`SoapySDRDevice', `SoapySDRStream', `CInt', fromIntegral `CLLong', `CULong'} -> `CInt'#}

foreign import ccall unsafe "SoapySDRDevice_readStream"
    readStream :: SoapySDRDevice -> SoapySDRStream -> Ptr (Ptr CFloat) -> CInt -> Ptr CInt -> Ptr CLLong -> CLong -> IO (CInt)

foreign import ccall unsafe "SoapySDRDevice_writeStream"
    writeStream :: SoapySDRDevice -> SoapySDRStream -> Ptr (Ptr CFloat) -> CULong -> Ptr CInt -> CLLong -> CLong -> IO (CInt)

{#fun unsafe SoapySDRDevice_lastError as ^
   {} -> `CString' #}

{#fun unsafe SoapySDRDevice_makeStrArgs as ^
    {`String'} -> `SoapySDRDevice'#}

{#fun unsafe SoapySDRDevice_unmake as ^
    {`SoapySDRDevice'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_deactivateStream as ^
    {`SoapySDRDevice', `SoapySDRStream', `CInt', fromIntegral `CLLong'} -> `CInt'#}

{#fun unsafe SoapySDRDevice_closeStream as ^
    {`SoapySDRDevice', `SoapySDRStream'} -> `CInt'#}

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

{#fun unsafe SoapySDRDevice_getNativeStreamFormat as ^
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CDouble' peek*} -> `String'#}

{#fun unsafe SoapySDRDevice_getStreamFormats as soapySDRDeviceGetStreamFormats'
    {`SoapySDRDevice' , `SoapySDRDirection', `SoapySDRChannel', alloca- `CULong' peek*} -> `Ptr CString' id#}

soapySDRDeviceGetStreamFormats dev dir chan = do
    y <-soapySDRDeviceGetStreamFormats' dev dir chan
    let len = fromIntegral (snd y)
    strPtrs <- peekArray len (fst y)
    mapM peekCString strPtrs
    