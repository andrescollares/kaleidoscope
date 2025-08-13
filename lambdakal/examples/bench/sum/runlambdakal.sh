# Cache "Warmup" run
time cabal run lambdakal -- -f ./examples/bench/sum/sum.lk -o0 | cut -c1-20

# Actual run
time cabal run lambdakal -- -f ./examples/bench/sum/sum.lk -o0 | cut -c1-20
