{-# LANGUAGE DerivingVia #-}

module DsuTest
  ( runDsuTest,
    writeLine,
    readLine,
    readLines,
    updateProgram,
    liftIO,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Lowarn.Runtime (Runtime, runRuntime)
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (WriteMode),
    hGetLine,
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    withFile,
  )
import System.Posix (ProcessID, forkProcess, sigSTOP, sigUSR2, signalProcess)
import System.Process (createPipe)
import qualified System.Timeout as Timeout (timeout)

data DsuTestData = DsuTestData
  { _inputHandle :: Handle,
    _outputHandle :: Handle,
    _fileHandle :: Handle,
    _processId :: ProcessID
  }

newtype DsuTest a = DsuTest (ReaderT DsuTestData IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO)
    via (ReaderT DsuTestData IO)

createPipeWithLineBuffering :: IO (Handle, Handle)
createPipeWithLineBuffering = do
  (readHandle, writeHandle) <- createPipe
  hSetBuffering readHandle LineBuffering
  hSetBuffering writeHandle LineBuffering
  return (readHandle, writeHandle)

runDsuTest ::
  DsuTest () ->
  ((Handle, Handle) -> Runtime ()) ->
  FilePath ->
  Int ->
  IO ()
runDsuTest (DsuTest reader) getRuntime outputPath timeout = do
  (inputReadHandle, inputWriteHandle) <- createPipeWithLineBuffering
  (outputReadHandle, outputWriteHandle) <- createPipeWithLineBuffering
  withFile outputPath WriteMode $ \fileHandle -> do
    hSetBinaryMode fileHandle True
    processId <-
      forkProcess $
        runRuntime $
          getRuntime (inputReadHandle, outputWriteHandle)

    timeoutResult <-
      Timeout.timeout timeout $
        runReaderT reader $
          DsuTestData inputWriteHandle outputReadHandle fileHandle processId

    signalProcess sigSTOP processId

    case timeoutResult of
      Just () -> return ()
      Nothing -> hPutStrLn fileHandle "Timeout reached"

writeLine :: String -> DsuTest ()
writeLine line = DsuTest $ do
  inputHandle <- asks _inputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    hPutStrLn inputHandle line
    hPutStrLn fileHandle $ "> " <> line

readLine :: DsuTest String
readLine = DsuTest $ do
  outputHandle <- asks _outputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    line <- hGetLine outputHandle
    hPutStrLn fileHandle line
    return line

readLines :: Int -> DsuTest [String]
readLines n = replicateM n readLine

updateProgram :: DsuTest ()
updateProgram = DsuTest $ do
  processId <- asks _processId
  liftIO $ do
    signalProcess sigUSR2 processId
    threadDelay 1000000
