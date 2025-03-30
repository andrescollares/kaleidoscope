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