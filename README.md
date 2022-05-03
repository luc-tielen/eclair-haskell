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
{-# LANGUAGE UndecidableInstances #-}
-- UndecidableInstances is only needed for the DerivingVia style API.

module Main ( main ) where

import qualified Language.Eclair as E
import GHC.Generics

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

## Integrating Eclair and Haskell

As of right now, the Eclair compiler outputs LLVM IR code that still needs to be
further compiled. With the LLVM toolchain, we can produce a library that GHC can
link with.

Follow the following steps (assuming `test.dl` contains your eclair program) to
generate a static archive containing the eclair program:

```bash
$ eclairc test.dl > test.ll
$ clang -c test.ll  # generates test.o
$ ar rcs libtest.a test.o
```

And now you can link with GHC (via cabal, stack, ...). Here's an example snippet
for a build that makes use of `hpack`:

```yaml
executables:
  eclair-example:
    source-dirs:      src
    main:             Main.hs
    dependencies:
      - eclair-haskell
    extra-lib-dirs: cbits  # this assumes the library is stored under "cbits/"
    extra-libraries: test
```
