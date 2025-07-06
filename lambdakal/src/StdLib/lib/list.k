def length([int] l) -> int: if l == [] then 0 else 1 + length(tail(l));
  
def all([int] l, fun (int) -> bool f) -> bool:
  if l == [] then
    true
  else
    (if f(head l) then
      all(tail l, f)
    else
      false);

def any([int] l, fun (int) -> bool f) -> bool:
  if l == [] then
    false
  else
    (if f(head l) then
      true
    else
      any(tail l, f));

def map_int([int] l, fun (int) -> int f) -> [int]:
  if l == [] then
    []
  else
    f(head l) : map_int(tail l, f);

def filter_int([int] l, fun (int) -> bool f) -> [int]:
  if l == [] then
    []
  else
    (if f(head l) then
      head l : filter_int(tail l, f)
    else
      filter_int(tail l, f));

def nth_int([int] l, int n) -> int:
  if n == 0 then
    head l
  else
    nth_int(tail l, n - 1);

def reverse_acc_int([int] l, [int] acc) -> [int]:
  if l == [] then
    acc
  else
    reverse_acc_int(tail l, head l : acc);

def reverse_int([int] l) -> [int]:
  reverse_acc_int(l, []);

def foldl_int([int] l, int acc, fun (int, int) -> int f) -> int:
  if l == [] then
    acc
  else
    foldl_int(tail l, f(acc, head l), f);

def foldr_int([int] l, int acc, fun (int, int) -> int f) -> int:
    if l == [] then
        acc
    else
        f(head l, foldr_int(tail l, acc, f));

def min_index_int([int] arr, int start) -> int:
  if start >= length(arr) - 1 then
    start
  else
    if nth_int(arr, start) < nth_int(arr, min_index_int(arr, start + 1)) then
      start
    else
      min_index_int(arr, start + 1);

def _internal_find_min_int([int] arr, int min_val, int min_idx, int acc) -> tuple(int, int):
  if arr == [] then
    (min_val, min_idx)
  else
    let current_val = head(arr) in
    if current_val < min_val then
      _internal_find_min_int(tail(arr), current_val, acc, acc + 1)
    else
      _internal_find_min_int(tail(arr), min_val, min_idx, acc + 1);

def find_min_int([int] arr, int min_val) -> tuple(int, int):
  _internal_find_min_int(arr, min_val, 0, 0);

def remove_nth_int([int] arr, int n) -> [int]:
  if n == 0 then
    tail(arr)
  else
    head(arr) : remove_nth_int(tail(arr), n - 1);

def selection_sort_int([int] arr) -> [int]:
  if arr == [] then
    []
  else
    let min_tuple = find_min_int(arr, 9999999) in
    let min_val = fst(min_tuple) in
    let min_idx = snd(min_tuple) in
    let rest = remove_nth_int(arr, min_idx) in
    min_val : selection_sort_int(rest);
