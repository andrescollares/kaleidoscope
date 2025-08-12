# Compile the Haskell code
ghc -main-is Fib examples/bench/fib/Fib.hs

# Warmup run
time ./examples/bench/fib/Fib | cut -c1-20

# Actual run
time ./examples/bench/fib/Fib | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/fib/Fib | cut -c1-20