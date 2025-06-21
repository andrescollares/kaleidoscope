def min_index([int] arr, int start) -> int:
  if start >= length(arr) - 1 then
    start
  else
    if nth_int(arr, start) < nth_int(arr, min_index(arr, start + 1)) then
      start
    else
      min_index(arr, start + 1);

def remove_nth([int] arr, int n) -> [int]:
  if n == 0 then
    tail(arr)
  else
    head(arr) : remove_nth(tail(arr), n - 1);

def selection_sort([int] arr) -> [int]:
  if length(arr) == 0 then
    []
  else
    let min_idx = min_index(arr, 0) in
    let min_val = nth_int(arr, min_idx) in
    let rest = remove_nth(arr, min_idx) in
    min_val : selection_sort(rest);

def rand_array(int n) -> [int]:
  if n == 0 then
    []
  else
    (rand() / 100000) : rand_array(n - 1);

selection_sort(rand_array(2000));