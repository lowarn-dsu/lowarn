{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module DsuTest
  ( DsuTest,
    runDsuTest,
    inputLine,
    outputLine,
    outputLines,
    writeInfo,
    updateProgram,
    liftIO,
    dsuGoldenTest,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Functor ((<&>))
import Lowarn.Runtime (Runtime, runRuntime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (WriteMode),
    hClose,
    hGetLine,
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    withFile,
  )
import System.Posix
  ( ProcessID,
    ProcessStatus (Exited, Stopped, Terminated),
    forkProcessWithUnmask,
    getProcessStatus,
    sigKILL,
    sigUSR2,
    signalProcess,
  )
import System.Process (createPipe)
import qualified System.Timeout as Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)
import Text.Printf (printf)

data DsuTestData = DsuTestData
  { _inputHandle :: Handle,
    _outputHandle :: Handle,
    _fileHandle :: Handle,
    _processId :: ProcessID
  }

newtype DsuTest a = DsuTest
  { unDsuTest :: ReaderT DsuTestData IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

data LogType = Input | Output | Error | Info
  deriving (Show)

writeLog :: Handle -> LogType -> String -> IO ()
writeLog handle logType = hPutStrLn handle . printf "%6s: %s" (show logType)

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
runDsuTest dsuTest getRuntime outputPath timeout = do
  (inputReadHandle, inputWriteHandle) <- createPipeWithLineBuffering
  (outputReadHandle, outputWriteHandle) <- createPipeWithLineBuffering
  withFile outputPath WriteMode $ \fileHandle -> do
    hSetBinaryMode fileHandle True
    hSetBuffering fileHandle LineBuffering

    processId <-
      forkProcessWithUnmask $ \unmask ->
        catch
          ( unmask $
              runRuntime $
                getRuntime (inputReadHandle, outputWriteHandle)
          )
          ( \exception ->
              writeLog fileHandle Error $
                displayException (exception :: SomeException)
          )
    Timeout.timeout
      timeout
      ( runReaderT (unDsuTest dsuTest) $
          DsuTestData inputWriteHandle outputReadHandle fileHandle processId
      )
      >>= \case
        Just () -> return ()
        Nothing ->
          writeLog fileHandle Error $
            printf "Timeout of %d microseconds reached." timeout

    getProcessStatusTimeout <-
      lookupEnv "CI"
        <&> ( \case
                Nothing -> normalGetProcessStatusTimeout
                Just "" -> normalGetProcessStatusTimeout
                Just _ -> ciGetProcessStatusTimeout
            )

    Timeout.timeout
      getProcessStatusTimeout
      ( getProcessStatus True True processId >>= \case
          Nothing -> do
            writeLog fileHandle Error "Process not available."
            signalProcess sigKILL processId
          Just (Exited ExitSuccess) -> return ()
          Just (Exited (ExitFailure exitCode)) ->
            writeLog fileHandle Error $
              printf "Process exited with exit code %d." exitCode
          Just (Terminated signal _) ->
            writeLog fileHandle Error $
              printf "Process terminated by signal %s." $
                show signal
          Just (Stopped signal) ->
            writeLog fileHandle Error $
              printf "Process stopped by signal %s." $
                show signal
      )
      >>= \case
        Just () -> return ()
        Nothing -> do
          writeLog fileHandle Error "Process did not end."
          signalProcess sigKILL processId

    hClose inputWriteHandle
  where
    normalGetProcessStatusTimeout = 1000000
    ciGetProcessStatusTimeout = 30000000

inputLine :: String -> DsuTest ()
inputLine line = DsuTest $ do
  inputHandle <- asks _inputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    hPutStrLn inputHandle line
    writeLog fileHandle Input line

outputLine :: DsuTest String
outputLine = DsuTest $ do
  outputHandle <- asks _outputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    line <- hGetLine outputHandle
    writeLog fileHandle Output line
    return line

outputLines :: Int -> DsuTest [String]
outputLines n = replicateM n outputLine

writeInfo :: String -> DsuTest ()
writeInfo info = DsuTest $ do
  fileHandle <- asks _fileHandle
  liftIO $ writeLog fileHandle Info info

updateProgram :: DsuTest ()
updateProgram = DsuTest $ do
  processId <- asks _processId
  fileHandle <- asks _fileHandle
  liftIO $ do
    signalProcess sigUSR2 processId
    writeLog fileHandle Info "Update signal sent."
    threadDelay 1000000

dsuGoldenTest ::
  String ->
  ((Handle, Handle) -> Runtime ()) ->
  DsuTest () ->
  Int ->
  TestTree
dsuGoldenTest testName getRuntime dsuTest timeout =
  goldenVsFileDiff
    testName
    (\a b -> ["diff", "-u", a, b])
    (testPath <.> "golden")
    (testPath <.> "log")
    $ runDsuTest dsuTest getRuntime (testPath <.> "log") timeout
  where
    testPath = "test" </> "golden" </> testName
