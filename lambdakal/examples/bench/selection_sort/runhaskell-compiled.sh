# Compile the Haskell code
ghc -main-is SelectionSort examples/bench/selection_sort/SelectionSort.hs

# Warmup run
time ./examples/bench/selection_sort/SelectionSort | cut -c1-20

# Actual run
time ./examples/bench/selection_sort/SelectionSort | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/selection_sort/SelectionSort | cut -c1-20