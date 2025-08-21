# Compile the Haskell code
ghc -main-is Sum examples/bench/sum/Sum.hs

# Warmup run
time ./examples/bench/sum/Sum | cut -c1-20

# Actual run
time ./examples/bench/sum/Sum | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/sum/Sum | cut -c1-20