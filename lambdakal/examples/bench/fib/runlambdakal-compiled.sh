# Build into executable with -c flag
cabal run lambdakal -- -f ./examples/bench/fib/fib.lk -o3 -c

# Cache "Warmup" run
time ./examples/bench/fib/fib | cut -c1-20
# Actual run (should be faster)
time ./examples/bench/fib/fib | cut -c1-20

# Memory usage
/usr/bin/time -v ./examples/bench/fib/fib | cut -c1-20