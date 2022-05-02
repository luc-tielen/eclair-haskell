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
data Edge
  = Edge Int32 Int32
  deriving (Generic)
  deriving anyclass Marshal

data Reachable
  = Reachable Int32 Int32
  deriving (Show, Generic)
  deriving anyclass Marshal

instance Fact Edge where
  type FactDirection Edge = 'Input
  factType = const 0  -- NOTE: for now, these constants needs to be looked up in the general LLVM code

instance Fact Reachable where
  type FactDirection Reachable = 'Output
  factType = const 1

example :: IO ()
example = do
  results <- withEclair Path $ \prog -> do
    E.addFacts prog [Edge 1 2, Edge 2 3]
    E.run prog
    E.getFacts prog
  process results
  where
    process :: [Reachable] -> IO ()
    process = traverse_ print
```
