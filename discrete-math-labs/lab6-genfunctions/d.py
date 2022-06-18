from fractions import *
from math import *


class Polynomial:
    c = list()

    def __init__(self, deg=None, c=None, norm=True):
        if deg is not None:
            self.c = [Fraction(0, 1) for i in range(deg + 1)]
        elif c is not None:
            self.c = c
            if norm:
                self.normalize()
        else:
            self.c = [Fraction(1, 1)]

    def normalize(self):
        deg = 0
        for i in range(len(self.c)):
            if self[i] != Fraction(0, 1):
                deg = i + 1
        self.c = self.c[0: deg]

    def deg(self):
        return len(self.c) - 1 if len(self.c) else 0

    def __len__(self):
        return len(self.c)

    def __getitem__(self, i):
        return self.c[i] if len(self) > i >= 0 else Fraction(0, 1)

    def add(self, o):
        p = [Fraction(0, 1) for i in range(max(len(self), len(o)))]

        for i in range(len(self)):
            p[i] += self[i]

        for i in range(len(o)):
            p[i] += o[i]

        return Polynomial(c=p)

    def multiply(self, o):
        p = [Fraction(0, 1) for i in range(len(self) + len(o))]

        for i in range(len(p)):
            for j in range(i + 1):
                if i - j >= len(o):
                    continue
                p[i] += self[j] * o[i - j]
        return Polynomial(c=p)

    def const_multiply(self, constant):
        p = [Fraction(0, 1) for i in range(len(self))]
        for i in range(len(self)):
            p[i] = self[i] * constant
        return Polynomial(c=p)

    def __str__(self):
        res = ""
        for i in self.c:
            res += "%s/%s " % (i.numerator, i.denominator)
        return res


def f_k(n_shift, k_arg):
    m = Fraction(1, factorial(k_arg - 1))
    quasi = Polynomial()

    for i in range(1, k_arg):
        term = Polynomial(c=[Fraction(i + n_shift, 1), Fraction(1, 1)])
        quasi = quasi.multiply(term)

    return quasi.const_multiply(m)


r, k = map(int, input().split())

P = Polynomial(c=[Fraction(int(i), 1) for i in input().split()])

processing = []
for i in range(len(P)):
    if P[i].numerator != 0:
        multiplier = Fraction(1, r**i) * P[i]
        processing.append(f_k(-i, k + 1).const_multiply(multiplier))

ans = Polynomial(deg=0)

for i in processing:
    ans = ans.add(i)

for j in [ans[i] for i in range(k + 1)]:
    print(str(j.numerator) + "/" + str(j.denominator), end=' ')
print()
