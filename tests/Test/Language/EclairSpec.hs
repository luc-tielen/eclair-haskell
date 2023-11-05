{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.EclairSpec
  ( module Test.Language.EclairSpec
  ) where

import Language.Haskell.TH.Syntax
import GHC.Generics
import Data.Word
import Data.Text
import Test.Hspec
import qualified Language.Eclair as E
import Language.Eclair
import qualified Language.Eclair.Internal as Internal
import Foreign (withForeignPtr)

data IOFact
  = IOFact Word32 Text
  deriving (Generic, Eq, Show)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions IOFact 'E.InputOutput "io"

data Edge
  = Edge Word32 Word32
  deriving (Generic, Eq, Show)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Edge 'E.Input "edge"

data Reachable
  = Reachable Word32 Word32
  deriving (Eq, Show, Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Reachable 'E.Output "reachable"

data EdgeStr
  = EdgeStr Text Text
  deriving (Generic, Eq, Show)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions EdgeStr 'E.Input "edge_str"

data ReachableStr
  = ReachableStr Text Text
  deriving (Eq, Show, Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions ReachableStr 'E.Output "reachable_str"

data Path = Path
  deriving E.Program
  via E.ProgramOptions Path
    '[ IOFact
     , Edge
     , Reachable
     , EdgeStr
     , ReachableStr
     ]

spec :: Spec
spec = describe "eclair-haskell" $ parallel $ do
  it "can encode and decode strings" $ do
    program <- Internal.init
    withForeignPtr program $ \progPtr -> do
      idx1 <- Internal.encodeString progPtr "io"
      idx2 <- Internal.encodeString progPtr "edge"
      idx3 <- Internal.encodeString progPtr "reachable"
      idx4 <- Internal.encodeString progPtr "edge_str"
      idx5 <- Internal.encodeString progPtr "reachable_str"
      idx6 <- Internal.encodeString progPtr "some_other_string"
      idx7 <- Internal.encodeString progPtr ""
      idx8 <- Internal.encodeString progPtr "some_other_string"
      [idx1, idx2, idx3, idx4, idx5, idx6, idx7, idx8]
        `shouldBe` [0, 1, 2, 3, 4, 5, 6, 5]
      str1 <- Internal.decodeString progPtr 0
      str2 <- Internal.decodeString progPtr 1
      str3 <- Internal.decodeString progPtr 2
      str4 <- Internal.decodeString progPtr 3
      str5 <- Internal.decodeString progPtr 4
      str6 <- Internal.decodeString progPtr 5
      str7 <- Internal.decodeString progPtr 6
      [str1, str2, str3, str4, str5, str6, str7] `shouldBe`
        [ "io"
        , "edge"
        , "reachable"
        , "edge_str"
        , "reachable_str"
        , "some_other_string"
        , ""
        ]

  it "can add and get facts to eclair" $ do
    facts1 <- withEclair Path E.getFacts
    facts1 `shouldBe` ([] :: [IOFact])
    facts2 <- withEclair Path $ \prog -> do
      E.addFacts prog [IOFact 1 "abc", IOFact 2 "def"]
      E.addFact prog $ IOFact 3 "ghijkl"
      E.run prog
      E.getFacts prog
    facts2 `shouldBe` [IOFact 1 "abc", IOFact 2 "def", IOFact 3 "ghijkl"]

  it "can run an eclair program with rules containing numbers" $ do
    results <- withEclair Path $ \prog -> do
      E.addFacts prog [Edge 1 2, Edge 2 3, Edge 3 4, Edge 6 7]
      E.run prog
      E.getFacts prog
    results `shouldBe`
      [ Reachable 1 2, Reachable 1 3, Reachable 1 4
      , Reachable 2 3, Reachable 2 4
      , Reachable 3 4
      , Reachable 6 7
      ]

  it "can run an eclair program with rules containing strings"  $ do
    results <- withEclair Path $ \prog -> do
      E.addFacts prog [EdgeStr "a" "b", EdgeStr "b" "cdef", EdgeStr "a" "ghijkl", EdgeStr "b" ""]
      E.run prog
      E.getFacts prog
    results `shouldBe`
      [ ReachableStr "a" "b", ReachableStr "a" "cdef", ReachableStr "a" "ghijkl", ReachableStr "a" ""
      , ReachableStr "b" "cdef", ReachableStr "b" ""
      ]

[] <$ qAddForeignFilePath RawObject "tests/fixtures/reachable.o"
