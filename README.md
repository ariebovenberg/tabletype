# XO

A toy tic-tac-toe program.

## Setup

```bash
cabal sandbox init
cabal build
```

## Tests

```bash
cabal configure --enable-tests
cabal install tasty tasty-hunit
cabal test
```
