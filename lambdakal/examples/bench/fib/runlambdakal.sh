# Cache "Warmup" run
time cabal run lambdakal -- -f ./examples/bench/fib/fib.lk -o3 | cut -c1-20
# Actual run (should be faster)
time cabal run lambdakal -- -f ./examples/bench/fib/fib.lk -o3 | cut -c1-20