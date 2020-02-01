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
-- {#enum define SoapySDRFormat {"CF32" as SOAPY_SDR_CF32, "CS32" as SOAPY_SDR_CS32} deriving (Eq,Show) #} -- Doesn't parse

{#pointer *SoapySDRDevice as SoapySDRDevice#}
{#pointer *SoapySDRStream as SoapySDRStream#}
{#pointer *SoapySDRKwargs as SoapySDRKwargs newtype#}

data SoapySDRRange = SoapySDRRange {
    minimum' :: CDouble,
    maximum' :: CDouble,
    step :: CDouble
} deriving Show
instance Storable SoapySDRRange where
  sizeOf _ = 24 
  alignment _ = 8
  peek ptr = do
      return (SoapySDRRange 0.0 0.0 0.0)
  poke ptr inp = do
        {#set SoapySDRRange.minimum #} ptr 1.0

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
    let strs = mapM peekCString strPtrs
    strs
    
