{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module                  : Lowarn.Cli.Retrofit.CommitMap
-- SPDX-License-Identifier : MIT
-- Stability               : stable
-- Portability             : non-portable (POSIX, GHC)
--
-- Module for mapping version numbers to Git commit IDs.
module Lowarn.Cli.Retrofit.CommitMap
  ( CommitMap,
    mkCommitMap,
    readCommitMap,
    lookupCommitMap,
  )
where

import ByteString.StrictBuilder
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text as Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Path
import Path.IO (doesFileExist)
import Text.Hex (decodeHex, encodeHex)

-- | A map from version numbers to Git commit IDs.
data CommitMap = CommitMap
  { commitMapByteString :: ByteString,
    commitMapIdBytes :: Int
  }

-- | Create a 'CommitMap' from a list of Git commit IDs, where each commit ID is
-- stored using the given number of bytes.
mkCommitMap :: [Text] -> Int -> Maybe CommitMap
mkCommitMap hexIds commitMapIdBytes =
  case builderBytes . mconcat <$> mIds of
    Just commitMapByteString -> Just CommitMap {..}
    Nothing -> Nothing
  where
    mIds :: Maybe [Builder]
    mIds = mapM (makeBuilder <=< decodeHex) hexIds

    makeBuilder :: ByteString -> Maybe Builder
    makeBuilder s =
      if ByteString.length s >= commitMapIdBytes
        then Just $ bytes $ ByteString.take commitMapIdBytes s
        else Nothing

-- | Create a 'CommitMap' from a commit map file, where each commit ID is stored
-- using the given number of bytes.
readCommitMap :: Path Abs File -> Int -> IO (Maybe CommitMap)
readCommitMap commitMapPath idBytes = do
  doesFileExist commitMapPath >>= \case
    False -> return Nothing
    True -> do
      hexIds <- Text.lines <$> Text.readFile (toFilePath commitMapPath)
      return $ mkCommitMap hexIds idBytes

slice :: Int -> Int -> ByteString -> ByteString
slice start len = ByteString.take len . ByteString.drop start

-- | Get the Git commit ID for the given version number from a 'CommitMap'.
lookupCommitMap :: CommitMap -> Int -> Maybe String
lookupCommitMap CommitMap {..} version =
  if ByteString.length commitMapByteString >= startIndex + commitMapIdBytes
    then
      Just $
        Text.unpack $
          encodeHex $
            slice startIndex commitMapIdBytes commitMapByteString
    else Nothing
  where
    startIndex = version * commitMapIdBytes
