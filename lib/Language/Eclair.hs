module Language.Eclair
  ( module Language.Eclair  -- TODO refactor exports
  ) where

-- TODO: move bindings to separate Internal/Bindings.hs file

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Exception

data Program
data Buffer  -- NOTE: u32 array

foreign import ccall unsafe "eclair_program_init" eclairProgramInit
  :: IO (Ptr Program)

foreign import ccall unsafe "&eclair_program_destroy" eclairProgramDestroy
  :: FunPtr (Ptr Program -> IO ())

foreign import ccall unsafe "eclair_program_run" eclairProgramRun
  :: Ptr Program -> IO ()

foreign import ccall unsafe "eclair_add_facts" eclairAddFacts
  :: Ptr Program -> CShort -> Ptr Buffer -> CSize -> IO ()

foreign import ccall unsafe "eclair_add_fact" eclairAddFact
  :: Ptr Program -> CShort -> Ptr Buffer -> IO ()

foreign import ccall unsafe "eclair_get_facts" eclairGetFacts
  :: Ptr Program -> CShort -> IO (Ptr Buffer)

foreign import ccall unsafe "&eclair_free_buffer" eclairFreeBuffer
  :: FunPtr (Ptr Buffer -> IO ())

foreign import ccall unsafe "eclair_fact_count" eclairFactCount
  :: Ptr Program -> CShort -> IO CSize

-- TODO move next code to Internal.hs

eclairInitialize :: IO (ForeignPtr Program)
eclairInitialize = mask_ $ do
  prog <- eclairProgramInit
  newForeignPtr eclairProgramDestroy prog

getFacts :: Ptr Program -> CShort -> IO (ForeignPtr Buffer)
getFacts prog factType = mask_ $ do
  buf <- eclairGetFacts prog factType
  newForeignPtr eclairFreeBuffer buf




-- TODO use typesystem to avoid errors
-- TODO typeclasses for marshalling data
-- TODO derivingvia to hide all boilerplate
