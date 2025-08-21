# Compile the Haskell code
ghc -main-is SumTailRecursive examples/bench/sum/SumTailRecursive.hs

# Warmup run
time ./examples/bench/sum/SumTailRecursive | cut -c1-20

# Actual run
time ./examples/bench/sum/SumTailRecursive | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/sum/SumTailRecursive | cut -c1-20