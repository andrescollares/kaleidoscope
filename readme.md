# LLVM+Haskell Kaleidoscope project

Andrés Collares & Agustín Gazzano

## Requirements for working on the project

- `stack`
- `ghc-9.2.4`
- [`llvm-hs`](https://hackage.haskell.org/package/llvm-hs)

## Linting and Formatting

- [`haskell-language-server`](https://github.com/haskell/haskell-language-server)
- [`ormolu`](https://github.com/tweag/ormolu)

## llvm-hs

> TODO: Check if building llvm-hs-15 from source is better than using stack or cabal, it could also open the possibility to make changes and bugfixes using a fork.

## Docker

```
docker compose build
docker compose up -d
docker compose run project bash
cabal run
```

### Available options

```
cabal run kaleidoscope-fing -- --help
```

### Running code samples

```
cabal run kaleidoscope-fing -- --file "./test/programs/recursive_fib.k"
```

### Test suite

```
cabal test
or to show progress:
cabal test tests --test-show-detail=streaming
```

### Running ghci through Cabal

```
cabal exec ghci
or
cabal repl
```

