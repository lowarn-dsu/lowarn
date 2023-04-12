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

-- $setup
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Data.Text (pack)

-- | A map from natural numbers to Git commit IDs.
data CommitMap = CommitMap
  { commitMapByteString :: ByteString,
    commitMapIdBytes :: Int
  }

-- | Create a 'CommitMap' from a list of Git commit IDs, where each commit ID is
-- stored using the given number of bytes. If any strings are not valid hex
-- strings (of bytes) or have fewer bytes than the requested size, 'Nothing'
-- is returned. If a commit ID has more bytes than the requested size, it is
-- truncated. If the requested size is zero or negative, 'Nothing' is returned.
--
-- ==== __Examples__
--
-- >>> isJust $ mkCommitMap [] 4
-- True
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 4
-- True
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 2
-- True
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 8
-- False
--
-- >>> isJust $ mkCommitMap (map pack ["4a293qf2", "61b0rie6"]) 4
-- False
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e61"]) 4
-- False
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 0
-- False
--
-- >>> isJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) (-1)
-- False
mkCommitMap :: [Text] -> Int -> Maybe CommitMap
mkCommitMap hexIds commitMapIdBytes =
  if commitMapIdBytes <= 0
    then Nothing
    else case builderBytes . mconcat <$> mIds of
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

-- | Get the Git commit ID for the given integer from a 'CommitMap', if it is in
-- range.
--
-- ==== __Examples__
--
-- >>> lookupCommitMap (fromJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 4) 0
-- Just "4a293bf2"
--
-- >>> lookupCommitMap (fromJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 4) 1
-- Just "61b093e6"
--
-- >>> lookupCommitMap (fromJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 4) 2
-- Nothing
--
-- >>> lookupCommitMap (fromJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 4) (-1)
-- Just "4a293bf2"
--
-- >>> lookupCommitMap (fromJust $ mkCommitMap (map pack ["4a293bf2", "61b093e6"]) 2) 1
-- Just "61b0"
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
