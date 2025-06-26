def rand_array(int n) -> [int]:
  if n == 0 then
    []
  else
    (rand() / 100000) : rand_array(n - 1);

selection_sort_int(rand_array(5000));
