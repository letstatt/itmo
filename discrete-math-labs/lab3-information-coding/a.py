n = int(input())
d = list(map(int, input().split()))
z = [(d[i], 1, i, None) for i in range(n)]
z.sort()

while len(z) > 1:
    z[1] = (z[0][0] + z[1][0], z[0][1] + z[1][1], z[0], z[1])
    z = sorted(z[1:])

ans = 0

def rec(t, res):
    global ans
    if t[1] == 1:
        ans += res * d[t[2]]
        return
    
    rec(t[2], res + 1)
    rec(t[3], res + 1)


rec(z[0], 0)
print(ans)