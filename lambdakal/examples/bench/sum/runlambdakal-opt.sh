# Cache "Warmup" run
time cabal run lambdakal -- -f ./examples/bench/sum/sum-acc.lk -o0 | cut -c1-20

# Actual run
time cabal run lambdakal -- -f ./examples/bench/sum/sum-acc.lk -o0 | cut -c1-20

# Memory
/usr/bin/time -v cabal run lambdakal -- -f ./examples/bench/sum/sum-acc.lk -o0 | cut -c1-20