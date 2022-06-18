s = input()
n = len(s)
d = dict()

for i in range(26):
    if chr(ord('a') + i) not in d:
        d[chr(ord('a') + i)] = i

x = s[0]

for i in range(1, n):
    t = x + s[i]
    
    if t in d:
        x = t
    else:
        print(d[x], end=' ')
        d[t] = len(d)
        x = s[i]


if x in d:
    print(d[x])
else:
    print(len(d))
