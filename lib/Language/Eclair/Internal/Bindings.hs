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
  ) where

import Foreign.C.Types
import Foreign.Ptr


data Program
data Buffer  -- NOTE: u32 array

foreign import ccall unsafe "eclair_program_init" eclairProgramInit
  :: IO (Ptr Program)

foreign import ccall unsafe "&eclair_program_destroy" eclairProgramDestroy
  :: FunPtr (Ptr Program -> IO ())

foreign import ccall unsafe "eclair_program_run" eclairProgramRun
  :: Ptr Program -> IO ()

foreign import ccall unsafe "eclair_add_facts" eclairAddFacts
  :: Ptr Program -> CUShort -> Ptr Buffer -> CSize -> IO ()

foreign import ccall unsafe "eclair_add_fact" eclairAddFact
  :: Ptr Program -> CUShort -> Ptr Buffer -> IO ()

foreign import ccall unsafe "eclair_get_facts" eclairGetFacts
  :: Ptr Program -> CUShort -> IO (Ptr Buffer)

foreign import ccall unsafe "&eclair_free_buffer" eclairFreeBuffer
  :: FunPtr (Ptr Buffer -> IO ())

foreign import ccall unsafe "eclair_fact_count" eclairFactCount
  :: Ptr Program -> CUShort -> IO CSize

