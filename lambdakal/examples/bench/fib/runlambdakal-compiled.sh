# Build into executable with -c flag
cabal run lambdakal -- -f ./examples/bench/fib/fib.lk -o0 -c

# Cache "Warmup" run
time ./examples/bench/fib/fib | cut -c1-20
# Actual run (should be faster)
time ./examples/bench/fib/fib | cut -c1-20