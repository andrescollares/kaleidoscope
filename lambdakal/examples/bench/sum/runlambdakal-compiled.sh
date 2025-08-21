# Build into executable with -c flag
cabal run lambdakal -- -f ./examples/bench/sum/sum.lk -o3 -c

# Cache "Warmup" run
time ./examples/bench/sum/sum | cut -c1-20
# Actual run (should be faster)
time ./examples/bench/sum/sum | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/sum/sum | cut -c1-20