def length_double([double] l) -> int: if l == [] then 0 else 1 + length_double(tail(l));
  
def all_double([double] l, fun (double) -> bool f) -> bool:
  if l == [] then
    true
  else
    (if f(head l) then
      all_double(tail l, f)
    else
      false);

def any_double([double] l, fun (double) -> bool f) -> bool:
  if l == [] then
    false
  else
    (if f(head l) then
      true
    else
      any_double(tail l, f));

def map_double([double] l, fun (double) -> double f) -> [double]:
  if l == [] then
    []
  else
    f(head l) : map_double(tail l, f);

def filter_double([double] l, fun (double) -> bool f) -> [double]:
  if l == [] then
    []
  else
    (if f(head l) then
      head l : filter_double(tail l, f)
    else
      filter_double(tail l, f));

def nth_double([double] l, double n) -> double:
  if n == 0 then
    head l
  else
    nth_double(tail l, n - 1);

def reverse_acc_double([double] l, [double] acc) -> [double]:
  if l == [] then
    acc
  else
    reverse_acc_double(tail l, head l : acc);

def reverse_double([double] l) -> [double]:
  reverse_acc_double(l, []);

def foldl_double([double] l, double acc, fun (double, double) -> double f) -> double:
  if l == [] then
    acc
  else
    foldl_double(tail l, f(acc, head l), f);

def foldr_double([double] l, double acc, fun (double, double) -> double f) -> double:
    if l == [] then
        acc
    else
        f(head l, foldr_double(tail l, acc, f));

def min_index_double([double] arr, int start) -> int:
  if start >= length_double(arr) - 1 then
    start
  else
    if nth_double(arr, start) < nth_double(arr, min_index_double(arr, start + 1)) then
      start
    else
      min_index_double(arr, start + 1);

def _internal_find_min_double([double] arr, double min_val, int min_idx, int acc) -> tuple(double, int):
  if arr == [] then
    (min_val, min_idx)
  else
    let current_val = head(arr) in
    if current_val < min_val then
      _internal_find_min_double(tail(arr), current_val, acc, acc + 1)
    else
      _internal_find_min_double(tail(arr), min_val, min_idx, acc + 1);

def find_min_double([double] arr, double min_val) -> tuple(double, int):
  _internal_find_min_double(arr, min_val, 0, 0);

def remove_nth_double([double] arr, int n) -> [double]:
  if n == 0 then
    tail(arr)
  else
    head(arr) : remove_nth_double(tail(arr), n - 1);

def selection_sort_double([double] arr) -> [double]:
  if arr == [] then
    []
  else
    let min_tuple = find_min_double(arr, 9999999) in
    let min_val = fst(min_tuple) in
    let min_idx = snd(min_tuple) in
    let rest = remove_nth_double(arr, min_idx) in
    min_val : selection_sort_double(rest);
