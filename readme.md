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
cabal repl
```
```
docker build -t kaleidoscope .
docker run -it kaleidoscope 
cabal build
cd /kaleidoscope/dist-newstyle/build/aarch64-linux/ghc-8.10.7/kaleidoscope-fing-0.1.0.0/x/kaleidoscope-fing/build/kaleidoscope-fing
./kaleidoscope-fing

o correr en repl
cabal repl
```

## Testing generated llvm

```
gcc foo.ll -o foo
./foo
```

Para ver el valor devuelto por el programa, se puede usar `echo $?`
(Nota: el valor devuelto se castea a un entero de 8 bits, sidevolvemos un float o double va a tomar los ultimos 8 bits de la mantisa)
