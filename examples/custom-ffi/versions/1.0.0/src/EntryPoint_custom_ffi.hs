{-# LANGUAGE CApiFFI #-}

module EntryPoint_custom_ffi (entryPoint) where

import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Lowarn
import System.IO
import Text.Printf

foreign import capi "stdlib.h abs" c_abs :: CInt -> IO CInt

foreign import capi "math.h log2" c_log2 :: CDouble -> IO CDouble

foreign import capi "crypt.h crypt" c_crypt :: CString -> CString -> IO CString

foreign import capi "triangle.h triangle" c_triangle :: CInt -> IO CInt

entryPoint :: EntryPoint Handle
entryPoint = EntryPoint $
  \runtimeData -> do
    let outHandle = fromMaybe stdout $ lastState runtimeData

    ten <- toInteger <$> c_abs (-10)
    hPutStrLn outHandle $ printf "abs(-10) = %d using stdlib.h" ten

    four <- (realToFrac <$> c_log2 16 :: IO Double)
    hPutStrLn outHandle $ printf "log2(16) = %f using math.h" four

    let key = "password123"
    let salt = "ab"
    password <-
      peekCString =<< withCString key (withCString salt . c_crypt)
    hPutStrLn outHandle $
      printf "crypt(\"%s\", \"%s\") = %s using crypt.h" key salt password

    six <- toInteger <$> c_triangle 3
    hPutStrLn outHandle $ printf "triangle(3) = %d using triangle.h" six

    return outHandle

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint Handle))

hsEntryPoint :: IO (StablePtr (EntryPoint Handle))
hsEntryPoint = newStablePtr entryPoint
