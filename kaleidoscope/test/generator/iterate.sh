iteration_count=$1
step=$2

for (( i=2; i <= $iteration_count; i+=$step ))
do
    echo "iteration $i"
    # generate program
    python3 ./test/generator/gen_programs.py $i
    # run program and time execution
    time cabal run kaleidoscope-fing "./test/generator/out.k"
done