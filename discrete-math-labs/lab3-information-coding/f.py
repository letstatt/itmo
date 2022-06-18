n = int(input())
z = list(map(int, input().split()))
d = dict()
s = ""

for i in range(26):
    d[i] = chr(ord('a') + i)

#print(d)
w = d[z[0]]
print(w, end='')

for i in range(1,n):
    entry = ""
    
    if z[i] in d:
        entry = d[z[i]]
    elif z[i] == len(d):
        entry = w + w[0]
    else:
        #print('bad')
        exit(1)
        break
    
    print(entry, end='')
    d[len(d)] = w + entry[0]
    w = entry

# dsafsarkarhira
# 13
# 3 18 0 5 27 17 10 0 17 7 8 17 0