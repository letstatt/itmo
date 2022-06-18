from random import randint
import sys

def binpow(a, b, mod):
    res = 1
    while b > 0:
        if b % 2 == 1:
            res = (res * a) % mod
        a = (a * a) % mod
        b //= 2
    return res

def test(a, d, m, n):
    b = binpow(a, m, n)
    if b == 1 or b == n - 1:
        return True
    for i in range(d-1):
        b = (b * b) % n
        if b == n - 1:
            return True
    return False

def solve(n):
    if n == 2 or n == 3:
        return True
    elif n == 1 or n % 2 == 0:
        return False
    else:
        d = 0
        m = n - 1

        while m % 2 == 0:
            m //= 2
            d += 1

        success = True

        for i in range(10):
            a = randint(2, n - 2)
            if not test(a, d, m, n):
                success = False
                break

        return success

n = int(sys.stdin.readline())

for k in range(n):
    sys.stdout.write("YES\n" if solve(int(sys.stdin.readline())) else "NO\n")
