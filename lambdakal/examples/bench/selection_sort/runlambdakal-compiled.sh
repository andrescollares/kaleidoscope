# Build into executable with -c flag
cabal run lambdakal -- -f ./examples/bench/selection_sort/selection_sort.k -o0 -c

# Cache "Warmup" run
time ./examples/bench/selection_sort/selection_sort | cut -c1-20
# Actual run (should be faster)
time ./examples/bench/selection_sort/selection_sort | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/selection_sort/selection_sort | cut -c1-20