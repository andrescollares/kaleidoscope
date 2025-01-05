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