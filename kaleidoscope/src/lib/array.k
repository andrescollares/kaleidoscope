def length([int] l) -> int: if l == [] then 0 else 1 + length(tail(l));

def includes_int([int] l, int elem) -> bool:
  if l == [] then 
    false
  else
    (if (head l) == elem then
      true
    else
      includes_int(tail l, elem));