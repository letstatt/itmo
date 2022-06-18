from fractions import *
from decimal import *
from math import log2

getcontext().prec = 1024

n = int(input())
s = input()

segments = [None for i in range(n)]
probabilities = [0 for i in range(n)]
l = Decimal(0)

for i in s:
    probabilities[ord(i) - ord('a')] += 1

for i in range(n):
    segments[i] = (l, l + Decimal(probabilities[i]) / Decimal(len(s)))
    l = segments[i][1]

l = Decimal(0)
r = Decimal(1)

for i in s:
    c = ord(i) - ord('a')
    newRight = l + (r - l) * segments[c][1]
    newLeft = l + (r - l) * segments[c][0]
    l = newLeft
    r = newRight

#print(l, r, (l + r) / 2)

q = ""

while l != 0:
    l, r = l * 2, r * 2
    tl, tr = str(int(l)), str(int(r))
    q = q + tr
    if tl != tr:
        break
    l -= int(l)
    r -= int(r)

print(n)
print(*probabilities)
print(q if len(q) else 0)
