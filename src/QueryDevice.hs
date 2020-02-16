{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Soapy
import Foreign.Ptr
import Options.Applicative

data Options = Options {
    arguments  :: String
}

optParser :: Parser Options
optParser = Options 
    <$> strOption (
        long "arguments"  
        <> short 'a' 
        <> metavar "ARGUMENTS" 
        <> help "Arguments to SoapySDR"
        <> showDefault
        <> value "driver=rtlsdr"
        )

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Receive and demodulate broadcast FM radio" <> header "RTLSDR FM")


queryDevice Options{..} = do
    dev <- soapySDRDeviceMakeStrArgs arguments
    formats <- soapySDRDeviceGetStreamFormats dev DirectionRX Channel0
    putStrLn ("stream formats:" ++ show formats)
    native <- soapySDRDeviceGetNativeStreamFormat dev DirectionRX Channel0
    putStrLn ("native stream format:" ++ show native)
    devStream <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    mtu <- soapySDRDeviceGetStreamMTU dev (snd devStream)
    putStrLn ("stream MTU:" ++ show mtu)
    closeResult <- soapySDRDeviceCloseStream dev (snd devStream)
    putStrLn ("close result:" ++ show closeResult)
    unmakeResult <- soapySDRDeviceUnmake dev
    putStrLn ("unmake result:" ++ show unmakeResult)

main = do
    op <- execParser opt
    queryDevice op