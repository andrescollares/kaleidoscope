import sys
sys.setrecursionlimit(50000)

def divisible_any(n: int, i: int) -> bool:
    if i < 2:
        return False
    if n % i == 0:
        return True
    return divisible_any(n, i - 1)

def is_prime(n: int) -> bool:
    return not divisible_any(n, n - 1)

def primes_until(n: int) -> list[int]:
    if n < 2:
        return []
    if not divisible_any(n, n - 1):
        return [n] + primes_until(n - 1)
    return primes_until(n - 1)

def main():
    primes = primes_until(25000)
    print(primes)

if __name__ == "__main__":
    main()