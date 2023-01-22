module Language.Eclair.Internal.Bindings
  ( Program
  , Buffer
  , eclairProgramInit
  , eclairProgramDestroy
  , eclairProgramRun
  , eclairAddFacts
  , eclairAddFact
  , eclairGetFacts
  , eclairFreeBuffer
  , eclairFactCount
  , eclairEncodeString
  , eclairDecodeString
  ) where

import Foreign.C.Types
import Foreign.Ptr

data Program
data Buffer -- NOTE: u32 array
data EclairString -- NOTE: const u8 array

foreign import ccall unsafe "eclair_program_init"
  eclairProgramInit
    :: IO (Ptr Program)

foreign import ccall unsafe "&eclair_program_destroy"
  eclairProgramDestroy
    :: FunPtr (Ptr Program -> IO ())

foreign import ccall unsafe "eclair_program_run"
  eclairProgramRun
    :: Ptr Program -> IO ()

foreign import ccall unsafe "eclair_add_facts"
  eclairAddFacts
    :: Ptr Program -> CUInt -> Ptr Buffer -> CSize -> IO ()

foreign import ccall unsafe "eclair_add_fact"
  eclairAddFact
    :: Ptr Program -> CUInt -> Ptr Buffer -> IO ()

foreign import ccall unsafe "eclair_get_facts"
  eclairGetFacts
    :: Ptr Program -> CUInt -> IO (Ptr Buffer)

foreign import ccall unsafe "&eclair_free_buffer"
  eclairFreeBuffer
    :: FunPtr (Ptr Buffer -> IO ())

foreign import ccall unsafe "eclair_fact_count"
  eclairFactCount
    :: Ptr Program -> CUInt -> IO CSize

foreign import ccall unsafe "eclair_encode_string"
  eclairEncodeString
    :: Ptr Program -> CUInt -> Ptr EclairString -> IO CUInt

foreign import ccall unsafe "eclair_decode_string"
  eclairDecodeString
    :: Ptr Program -> CUInt -> IO (Ptr EclairString)
