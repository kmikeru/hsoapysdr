import Control.Monad
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import Data.Complex
import Foreign.C.Types
import TestSoapy
import Data.Coerce (coerce)
import System.Mem (performGC)

import SDR.Filter 
import SDR.Util
import SDR.Demod
import SDR.Pulse
import SDR.CPUID
import Coeffs
import qualified Data.Vector.Storable as VS

rcvstream :: Producer (VS.Vector (Complex Float)) IO ()
rcvstream = do
    dev <- lift setupStream
    forever $ do
        samples <- lift (consumeStream dev)
        let compl = toComplexf samples
        Pipes.yield (VS.fromList compl)

nsamples = 1024
main =  do
    info <- getCPUInfo
    let str = rcvstream
     
    sink <- pulseAudioSink

    deci <- fastDecimatorC info 8 coeffsRFDecim 
    resp <- fastResamplerR info 3 10 coeffsAudioResampler
    filt <- fastFilterSymR info coeffsAudioFilter

    runEffect $ str
        >-> firDecimator deci nsamples
        >-> fmDemod
        >-> firResampler resp nsamples
        >-> firFilter filt nsamples
        >-> sink