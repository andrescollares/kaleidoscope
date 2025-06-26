# Cache "Warmup" run
time runhaskell ./examples/bench/fib/Fib.hs | cut -c1-20
# Actual run (should be faster)
time runhaskell ./examples/bench/fib/Fib.hs | cut -c1-20