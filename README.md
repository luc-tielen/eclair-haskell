# eclair-haskell

Haskell bindings for [Eclair Datalog](https://github.com/luc-tielen/eclair-lang.git).

## Motivating example

Given the following Eclair program:

```prolog
@def edge(u32, u32).
@def path(u32, u32).

path(x, y) :-
  edge(x, y).

path(x, z) :-
  edge(x, y),
  path(y, z).
```

You can create the following Haskell program to bind to Eclair and serialize
data back and forth between Haskell and Eclair:

```haskell
{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia #-}

module Main ( main ) where

import qualified Language.Eclair as E

-- NOTE: for now, the constants 0 and 1 needs to be looked up in the generated LLVM code

data Edge
  = Edge Int32 Int32
  deriving (Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Edge 'E.Input 0

data Reachable
  = Reachable Int32 Int32
  deriving (Show, Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Reachable 'E.Output 1

data Path = Path
  deriving E.Program
  via E.ProgramOptions Path '[Edge, Reachable]

main :: IO ()
main = do
  results <- E.withEclair Path $ \prog -> do
    E.addFacts prog [Edge 1 2, Edge 2 3]
    E.addFact prog $ Edge 4 5
    E.run prog
    E.getFacts prog
  process results
  where
    process :: [Reachable] -> IO ()
    process = traverse_ print
```
