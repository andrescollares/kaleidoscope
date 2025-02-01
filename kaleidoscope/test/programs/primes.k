def divisibleAny(int n, int i) -> bool:
    if i < 2 then
        false
    else
        if n % i == 0 then
            true
        else
            divisibleAny(n, i - 1);
        

def isPrime(int n) -> bool: !divisibleAny(n, n - 1);

def primesUntil(int n) -> [int]:
    if n < 2 then
        []
    else
        if isPrime(n) then
            n:primesUntil(n - 1)
        else
            primesUntil(n - 1);

primesUntil(50);