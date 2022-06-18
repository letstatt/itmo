import math


class Polynomial:
    c = list()

    def __init__(self, deg=None, c=None, norm=True):
        if deg is not None:
            self.c = [0 for i in range(deg + 1)]
        elif c is not None:
            self.c = c
            if norm:
                self.__normalize()
        else:
            self.c = [1]

    def __normalize(self):
        deg = 0
        for i in range(len(self.c)):
            if self[i] != 0:
                deg = i + 1
        self.c = self.c[0: deg]

    def normalize(self):
        return Polynomial(c=self.c)

    def deg(self):
        return len(self.c) - 1 if len(self.c) else 0

    def __len__(self):
        return len(self.c)

    def __getitem__(self, i):
        return self.c[i] if len(self) > i >= 0 else 0

    def add(self, o, norm=True):
        p = [0 for i in range(max(len(self), len(o)))]

        for i in range(len(self)):
            p[i] += self[i]

        for i in range(len(o)):
            p[i] += o[i]

        return Polynomial(c=p, norm=norm)

    def multiply(self, o, norm=True):
        p = [0 for i in range(len(self) + len(o))]

        for i in range(len(p)):
            for j in range(i + 1):
                if i - j >= len(o):
                    continue
                p[i] += self[j] * o[i - j]
        return Polynomial(c=p, norm=norm)

    def const_multiply(self, constant):
        p = [0 for i in range(len(self))]
        for i in range(len(self)):
            p[i] = self[i] * constant
        return Polynomial(c=p)

    def div(self, o, d, norm=True):
        p = [0 for i in range(d)]
        assert o[0] == 1

        for i in range(d):
            p[i] = self[i]
            for j in range(i):
                p[i] -= p[j] * o[i - j]

        return Polynomial(c=p, norm=norm)

    def mod(self, k):
        p = [self[i] for i in range(k)]
        return Polynomial(c=p)

    def negate(self):
        p = [-i for i in self.c]
        return Polynomial(c=p, norm=False)

    def pow(self, n, mod, norm=True):
        res = Polynomial()
        a = Polynomial(c=self, norm=False)
        while n > 0:
            if n & 1:
                res = res.multiply(a, norm=norm).mod(mod)
            a = a.multiply(a, norm=norm).mod(mod)
            n //= 2
        return res

    def __str__(self):
        return " ".join(map(str, self.c))

    def __repr__(self):
        return "{" + ", ".join(*[map(str, self.c)][:10]) + "}"


def f_k(k_arg):
    quasi = Polynomial()

    for i in range(1, k_arg + 1):
        term = Polynomial(c=[i, 1])
        quasi = quasi.multiply(term)

    return quasi


r = int(input())
d = int(input())
D = Polynomial(c=[int(i) for i in input().split()])

P = Polynomial(deg=d)
Q = Polynomial()

for i in range(d, -1, -1):
    coeff = D[i]
    D = D.add(f_k(i).const_multiply(-coeff), norm=False)
    P = P.add(Q.const_multiply(coeff * math.factorial(i)))
    Q = Q.multiply(Polynomial(c=[1, -r]))

print(P.deg())
print(P)
print(Q.deg())
print(Q)
