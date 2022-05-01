module Language.Eclair.Internal
  ( Bindings.Program
  , Bindings.Buffer
  , init
  , run
  , getFacts
  , addFacts
  , addFact
  , factCount
  ) where

import Prelude hiding (init)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Word
import Control.Exception
import qualified Language.Eclair.Internal.Bindings as Bindings

-- TODO move next code to Internal.hs

init :: IO (ForeignPtr Bindings.Program)
init = mask_ $ do
  prog <- Bindings.eclairProgramInit
  newForeignPtr Bindings.eclairProgramDestroy prog

run :: Ptr Bindings.Program -> IO ()
run = Bindings.eclairProgramRun

getFacts :: Ptr Bindings.Program -> Word16 -> IO (ForeignPtr Bindings.Buffer)
getFacts prog factType = mask_ $ do
  buf <- Bindings.eclairGetFacts prog (CUShort factType)
  newForeignPtr Bindings.eclairFreeBuffer buf

addFacts :: Ptr Bindings.Program -> Word16 -> Ptr Bindings.Buffer -> Word64 -> IO ()
addFacts prog factType buf count =
  Bindings.eclairAddFacts prog (CUShort factType) buf (CSize count)

addFact :: Ptr Bindings.Program -> Word16 -> Ptr Bindings.Buffer -> IO ()
addFact prog factType buf =
  Bindings.eclairAddFact prog (CUShort factType) buf

factCount :: Ptr Bindings.Program -> Word16 -> IO Word64
factCount prog factType = do
  CSize count <- Bindings.eclairFactCount prog (CUShort factType)
  pure count

