n = int(input())
a = []
b = []

for i in range(n):
    x, y = map(int, input().split())
    a.append(x)
    b.append(y)

ra = {}
rb = {}

for i, j in zip(sorted(a), sorted(b)):
    ra[i] = len(ra) + 1
    rb[j] = len(rb) + 1

d_sum = 0

for i, j in zip(a, b):
    d_sum += (ra[i] - rb[j])**2

print(1 - 6 * d_sum / (n**3 - n))
