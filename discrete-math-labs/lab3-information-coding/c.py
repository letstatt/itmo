s = input()
n = len(s)
d = ["" for i in range(n)]

for i in range(n):
    for j in range(n):
        d[j] = s[j] + d[j]
    d.sort()

print(d[0])