{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module                  : Test.Lowarn.Story
-- SPDX-License-Identifier : MIT
-- Stability               : experimental
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for running integration tests with Lowarn.
module Test.Lowarn.Story
  ( -- * Monad
    Story,
    runStory,
    liftIO,

    -- * Actions
    inputLine,
    outputLine,
    outputLines,
    writeInfo,
    updateProgram,

    -- * Tasty integration
    storyGoldenTest,
  )
where

import Control.Concurrent (threadDelay, withMVar)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.IORef (newIORef, readIORef, writeIORef)
import Lowarn.Runtime (Runtime, runRuntime)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
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
import Test.Lowarn.Golden (goldenTest)
import Test.Lowarn.Tasty (BinarySemaphore)
import Test.Tasty (TestTree)
import Text.Printf (printf)

data StoryData = StoryData
  { _inputHandle :: Handle,
    _outputHandle :: Handle,
    _fileHandle :: Handle,
    _processId :: ProcessID
  }

-- | Monad for stories, which are sequences of actions to be run on a program
-- that is running with Lowarn's runtime.
newtype Story a = Story
  { unStory :: ReaderT StoryData IO a
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

-- | Run a story, producing a log file using input and output handles. This
-- function must be given a function that takes a pair of input and output
-- handles and returns a runtime. This function must also be given a file path
-- for the log file, and a timeout in microseconds.
--
-- Examples of the format of the log file can be found in Lowarn's test suite.
runStory ::
  -- | A story.
  Story () ->
  -- | A function that takes a pair of input and output handles and returns a
  -- runtime.
  ((Handle, Handle) -> Runtime ()) ->
  -- | The file path of the output log file.
  FilePath ->
  -- | A timeout in microseconds.
  Int ->
  IO ()
runStory story getRuntime outputPath timeout = do
  (inputReadHandle, inputWriteHandle) <- createPipeWithLineBuffering
  (outputReadHandle, outputWriteHandle) <- createPipeWithLineBuffering
  withFile outputPath WriteMode $ \fileHandle -> do
    hSetBinaryMode fileHandle True
    hSetBuffering fileHandle LineBuffering

    shouldWriteExceptionsRef <- newIORef True

    processId <-
      forkProcessWithUnmask $ \unmask ->
        catch
          ( unmask $
              runRuntime
                ( getRuntime (inputReadHandle, outputWriteHandle)
                )
                False
          )
          ( \exception -> do
              shouldWriteExceptions <- readIORef shouldWriteExceptionsRef
              when shouldWriteExceptions $
                writeLog fileHandle Error $
                  displayException (exception :: SomeException)
          )

    Timeout.timeout
      timeout
      ( runReaderT (unStory story) $
          StoryData inputWriteHandle outputReadHandle fileHandle processId
      )
      >>= \case
        Just () -> return ()
        Nothing -> writeLog fileHandle Error "Timeout reached."

    getProcessStatus False True processId
      >>= \case
        Nothing -> return ()
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

    writeIORef shouldWriteExceptionsRef False
    signalProcess sigKILL processId

    hClose inputWriteHandle

-- | Action that queues a line to be read by the program.
inputLine :: String -> Story ()
inputLine line = Story $ do
  inputHandle <- asks _inputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    hPutStrLn inputHandle line
    writeLog fileHandle Input line

-- | Action that blocks until a line can be read from the program.
outputLine :: Story String
outputLine = Story $ do
  outputHandle <- asks _outputHandle
  fileHandle <- asks _fileHandle
  liftIO $ do
    line <- hGetLine outputHandle
    writeLog fileHandle Output line
    return line

-- | Action that blocks until a given number of lines can be read from the
-- program.
outputLines :: Int -> Story [String]
outputLines n = replicateM n outputLine

-- | Action that writes a string to the log file.
writeInfo :: String -> Story ()
writeInfo info = Story $ do
  fileHandle <- asks _fileHandle
  liftIO $ writeLog fileHandle Info info

-- | Action that sends an update signal to the program. This action waits for
-- 1 second after sending the update signal, so it can be received.
updateProgram :: Story ()
updateProgram = Story $ do
  processId <- asks _processId
  fileHandle <- asks _fileHandle
  liftIO $ do
    signalProcess sigUSR2 processId
    writeLog fileHandle Info "Update signal sent."
    threadDelay 1000000

-- | Run a test that compares a golden file to the output of running a story
-- with a runtime, once a binary semaphore has been acquired.
--
-- The location of the golden and log files is
-- @PACKAGE_DIR\/test\/golden\/TEST_NAME.{log,golden}@, where @PACKAGE_DIR@ is
-- the directory of the package that is being run and @TEST_NAME@ is the given
-- name of the test.
storyGoldenTest ::
  -- | The name of the test, which is also used to determine the location of
  -- files.
  String ->
  -- | A function that takes a pair of input and output handles and returns a
  -- runtime.
  ((Handle, Handle) -> Runtime ()) ->
  -- | A story.
  Story () ->
  -- | A timeout in microseconds.
  Int ->
  -- | An action that returns a binary semaphore.
  IO BinarySemaphore ->
  TestTree
storyGoldenTest testName getRuntime story timeout binarySemaphoreAction =
  goldenTest
    testName
    $ \logFile -> do
      binarySemaphore <- binarySemaphoreAction
      withMVar binarySemaphore $
        const $
          runStory story getRuntime logFile timeout
