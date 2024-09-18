def length([int] l) -> int: if l == [] then 0 else 1 + length(tail(l));

def every([int] l, fun (int) -> bool f) -> bool:
  if l == [] then
    true
  else
    (if f(head l) then
      every(tail l, f)
    else
      false);