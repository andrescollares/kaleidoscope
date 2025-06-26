def find_min([int] arr, int min_val, int min_idx, int len, int acc) -> tuple(int, int):
  if len == 0 then
    (min_val, min_idx)
  else
    let current_val = head(arr) in
    if current_val < min_val then
      find_min(tail(arr), current_val, acc, len - 1, acc + 1)
    else
      find_min(tail(arr), min_val, min_idx, len - 1, acc + 1);

def remove_nth([int] arr, int n) -> [int]:
  if n == 0 then
    tail(arr)
  else
    head(arr) : remove_nth(tail(arr), n - 1);

def selection_sort_internal([int] arr, int len) -> [int]:
  if len == 0 then
    []
  else
    let min_tuple = find_min(arr, 99999, 0, length(arr), 0) in
    let min_val = fst(min_tuple) in
    let min_idx = snd(min_tuple) in
    let rest = remove_nth(arr, min_idx) in
    min_val : selection_sort_internal(rest, len - 1);

def selection_sort([int] arr) -> [int]:
  let len = length(arr) in
  selection_sort_internal(arr, len);

def rand_array(int n) -> [int]:
  if n == 0 then
    []
  else
    (rand() / 100000) : rand_array(n - 1);

selection_sort(rand_array(1000));
