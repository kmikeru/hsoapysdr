import Control.Monad
import Control.Monad.Trans.Except
import Control.Error.Util
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Graphics.Rendering.OpenGL
import Data.Complex
import Foreign.C.Types
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Window
import TestSoapy
import Data.Coerce (coerce)
import System.Mem (performGC)

randomVect :: Producer [GLfloat] IO ()
randomVect =  P.repeatM $ do
    res <- replicateM 1024 randomIO
    threadDelay 1000
    return res

fftpipe :: Producer [GLfloat] IO ()
fftpipe = do
    dev <- lift setupStream
    forever $ do
        samples <- lift (consumeStream dev)
        let compl = toComplex samples
        ffts <- lift (fft compl)
        let reals = map (realToFrac . realPart) ffts
        lift $ performGC
        Pipes.yield reals

fftpipe2 :: Producer [GLfloat] IO ()
fftpipe2 = do
    dev <- lift setupStream
    (inA, outA, plan) <- lift $ fftsetup
    forever $ do
        samples <- lift (consumeStream dev)
        let compl = toComplex samples
        ffts <- lift (presetfft inA outA plan compl)
        let reals = map (realToFrac . realPart) ffts
        lift $ performGC
        Pipes.yield reals

randfftpipe :: Producer [GLfloat] IO ()
randfftpipe = P.repeatM $ do
    samples <- replicateM 1024 randomIO
    threadDelay 1000
    let compl = toComplex samples
    ffts <- fft compl
    let reals = map (realToFrac . realPart) ffts
    return (reals)

main = exceptT putStrLn return $ do
    res <- lift setupGLFW
    unless res (throwE "Unable to initilize GLFW")

    waterfall <- window 1024 480 $ renderWaterfall 1024 1000 jet_mod

    lift $ runEffect $ fftpipe2 >-> waterfall