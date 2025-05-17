iteration_count=$1
step=$2

for (( i=2; i <= $iteration_count; i+=$step ))
do
    echo "iteration $i"
    # generate program
    runhaskell ./test/generator/GenPrograms.hs $i
    # run program and time execution
    time cabal run lambdakal "./test/generator/out.k"
done