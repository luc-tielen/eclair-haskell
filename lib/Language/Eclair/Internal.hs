module Language.Eclair.Internal
  ( Bindings.Program
  , Bindings.Buffer
  , init
  , run
  , getFacts
  , addFacts
  , addFact
  , factCount
  , encodeString
  , decodeString
  ) where

import Prelude hiding (init)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Exception
import qualified Language.Eclair.Internal.Bindings as Bindings


init :: IO (ForeignPtr Bindings.Program)
init = mask_ $ do
  prog <- Bindings.eclairProgramInit
  newForeignPtr Bindings.eclairProgramDestroy prog

run :: Ptr Bindings.Program -> IO ()
run = Bindings.eclairProgramRun

getFacts :: Ptr Bindings.Program -> Word32 -> IO (ForeignPtr Bindings.Buffer)
getFacts prog factType = mask_ $ do
  buf <- Bindings.eclairGetFacts prog (CUInt factType)
  newForeignPtr Bindings.eclairFreeBuffer buf

addFacts :: Ptr Bindings.Program -> Word32 -> Ptr Bindings.Buffer -> Word64 -> IO ()
addFacts prog factType buf count =
  Bindings.eclairAddFacts prog (CUInt factType) buf (CSize count)

addFact :: Ptr Bindings.Program -> Word32 -> Ptr Bindings.Buffer -> IO ()
addFact prog factType buf =
  Bindings.eclairAddFact prog (CUInt factType) buf

factCount :: Ptr Bindings.Program -> Word32 -> IO Word64
factCount prog factType = do
  CSize count <- Bindings.eclairFactCount prog (CUInt factType)
  pure count

encodeString :: Ptr Bindings.Program -> Text -> IO Word32
encodeString prog str = do
  let utf8Data = TE.encodeUtf8 str
      utf8Length = CUInt $ fromIntegral $ BS.length utf8Data
  BSU.unsafeUseAsCString utf8Data $ \utf8Ptr -> do
    CUInt index <- Bindings.eclairEncodeString prog utf8Length (castPtr utf8Ptr)
    pure index

decodeString :: Ptr Bindings.Program -> Word32 -> IO Text
decodeString prog index = do
  symbolPtr <- Bindings.eclairDecodeString prog (CUInt index)
  if symbolPtr == nullPtr
    then pure ""
    else do
      len <- peek (castPtr symbolPtr :: Ptr Word32)
      let utf8Ptr = symbolPtr `plusPtr` 4
      stringPtr <- peek (castPtr utf8Ptr :: Ptr (Ptr CChar))
      bs <- BSU.unsafePackCStringLen (stringPtr, fromIntegral len)
      pure $ TE.decodeUtf8 bs
