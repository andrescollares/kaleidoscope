FILE=./examples/bench/sum/sum.lk

# remove stack limit
# ulimit -s unlimited
  
# Cache "Warmup" run
time cabal run lambdakal -- -f "$FILE" -o0 > /dev/null 2>&1

for N in 1000.0 10000.0 100000.0 1000000.0 10000000.0 100000000.0; do
  for OPT in 0 3; do
    echo "Running with N=$N and optimization level $OPT"
    awk -v LINE="sum(one_to_n($N));" '
      NR>1 {print prev}
      {prev=$0}
      END {print LINE}
    ' "$FILE" > "$FILE.tmp"

    time cabal run lambdakal -- -f "$FILE.tmp" -o"$OPT" 2>&1 | grep "real"
    /usr/bin/time -v cabal run lambdakal -- -f "$FILE.tmp" -o"$OPT" 2>&1 | grep "Maximum resident set size"
    sleep 1  # Sleep to avoid too fast consecutive runs
  done
done