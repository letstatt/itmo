from fractions import *
from decimal import *
from math import log2

getcontext().prec = 1024

n = int(input())
probabilities = list(map(int, input().split()))
m = sum(probabilities)
qb = input()
q = Decimal(0)

for i in qb:
    q *= 2
    if i == '0':
        q += 0
    else:
        q += 1
q /= Decimal(2**len(qb))
#print(q)

segments = [None for i in range(n)]
l = Decimal(0)

for i in range(n):
    segments[i] = (l, l + Decimal(probabilities[i]) / Decimal(m))
    l = segments[i][1]

l = Decimal(0)
r = Decimal(1)
s = ""

for i in range(m):
    for j in range(n):
        if q >= segments[j][0] and q < segments[j][1]:
            s += chr(ord('a') + j)
            q = (q - segments[j][0]) / (segments[j][1] - segments[j][0])
            break

print(s)
