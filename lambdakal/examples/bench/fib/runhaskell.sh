# Cache "Warmup" run
time runhaskell ./examples/bench/fib/Fib.hs | cut -c1-20
# Actual run (should be faster)
time runhaskell ./examples/bench/fib/Fib.hs | cut -c1-20

# Memory usage
/usr/bin/time -v runhaskell ./examples/bench/fib/Fib.hs | cut -c1-20
