s, t = "", input()
n = len(t)
d = []

for i in range(n):
    d.append(t + s)
    s += t[0]
    t = t[1:]

d.sort()
r = [i[-1] for i in d]
print(''.join(r))