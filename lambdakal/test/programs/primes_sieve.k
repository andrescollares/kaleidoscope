def divisible(int n, int i) -> bool:
  n % i == 0;

def filter_sieve([int] l, int f) -> [int]:
  if length(l) == 0 then
    []
  else
    if divisible(head l, f) then
      filter_sieve(tail l, f)
    else
      head l : filter_sieve(tail l, f);

def sieve([int] l) -> [int]:
  if length(l) == 0 then
    []
  else
    let f = head l in
    f : sieve(filter_sieve(tail l, f));

def two_to_n(int n) -> [int]:
  if n == 2 then
    [2]
  else
    n : two_to_n(n - 1);

def primes_fast(int n) -> [int]:
  sieve(reverse_int(two_to_n(n)));


primes_fast(50);
