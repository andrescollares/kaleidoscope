# Cache "Warmup" run
time runhaskell ./examples/bench/SelectionSort.hs | cut -c1-20
# Actual run (should be faster)
time runhaskell ./examples/bench/SelectionSort.hs | cut -c1-20