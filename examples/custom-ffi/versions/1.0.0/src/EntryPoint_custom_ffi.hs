{-# LANGUAGE CApiFFI #-}

module EntryPoint_custom_ffi (entryPoint) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Lowarn
import Text.Printf

foreign import capi "stdlib.h abs" c_abs :: CInt -> IO CInt

foreign import capi "math.h log2" c_log2 :: CDouble -> IO CDouble

foreign import capi "crypt.h crypt" c_crypt :: CString -> CString -> IO CString

entryPoint :: EntryPoint ()
entryPoint = EntryPoint $
  const $ do
    ten <- toInteger <$> c_abs (fromInteger $ -10)
    putStrLn $ printf "abs(-10) = %d using stdlib.h" ten

    four <- (realToFrac <$> c_log2 (fromInteger $ 16) :: IO Double)
    putStrLn $ printf "log2(16) = %f using math.h" four

    let key = "password123"
    let salt = "ab"
    password <-
      peekCString =<< withCString key (withCString salt . c_crypt)
    putStrLn $
      printf "crypt(\"%s\", \"%s\") = %s using crypt.h" key salt password

foreign export ccall "hs_entryPoint_v1v0v0"
  hsEntryPoint :: IO (StablePtr (EntryPoint ()))

hsEntryPoint :: IO (StablePtr (EntryPoint ()))
hsEntryPoint = newStablePtr entryPoint
