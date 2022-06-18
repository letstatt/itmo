from string import ascii_lowercase

s = input()
a = [i for i in ascii_lowercase]

for i in s:
    ind = a.index(i)
    z = a[ind]
    a.remove(i)
    a = [z] + a
    print(ind + 1, end=' ')
