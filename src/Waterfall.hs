import Control.Monad
import Control.Monad.Trans.Except
import Control.Error.Util
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Data.Complex
import Foreign.C.Types
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Window
import Graphics.Rendering.OpenGL
import SoapySDRUtils
import System.Mem (performGC)

fftpipe :: Producer [GLfloat] IO ()
fftpipe = do
    dev <- lift setupStream
    (inA, outA, plan) <- lift fftsetup
    forever $ do
        samples <- lift (consumeStream dev)
        let compl = toComplex samples
        ffts <- lift (presetfft inA outA plan compl)
        let reals = map (realToFrac . magnitude) ffts
        lift performGC
        Pipes.yield reals

main = exceptT putStrLn return $ do
    res <- lift setupGLFW
    unless res (throwE "Unable to initilize GLFW")

    waterfall <- window 1024 480 $ renderWaterfall 1024 1000 jet_mod

    lift $ runEffect $ fftpipe >-> waterfall