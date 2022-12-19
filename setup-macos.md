https://github.com/sdiehl/kaleidoscope
https://www.stephendiehl.com/llvm/

# 1. Install GHCup

install GHCup: `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`


# 2. Install LLVM

`arch -arm64 brew install llvm@11`
`LLVM_CONFIG="/opt/homebrew/Cellar/llvm@11/11.1.0_4/bin/llvm-config" arch -arm64 pip install llvmlite`

- probar `python3.10 -m pip` en vez de `pip` si no anda

## 2.1 (Optional) run hello world with llc compiler

- `clang -O3 -emit-llvm hello.c -c -o hello.bc`
- `llc hello.bc -o hello.s`
- `gcc hello.s -o hello.native `
- `./hello`


# 3. Correr Kaleidoscope (ghci)

## 3.1 ghci

```
:l Main
main
```

## 3.2 ghc

```
ghc Main
./Main
```

## 3.3 Ejemplo del capitulo 2

```
ready> def foo(x y) x+foo(y, 4.0);
Function "foo" [Var "x",Var "y"] (BinOp Plus (Var "x") (Call "foo" [Var "y",Float 4.0]))
```

# 4. Docker (wip)

Se puede usar amd64 o arm64. El Dockerfile esta configurado con amd64 por ahora.

- docker build -t kaleidoscope-amd64 .
- docker run -it kaleidoscope-amd64











