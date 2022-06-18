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


def op_l(p):
    if len(p) > 0:
        p.c[0] = 0  # warn: source modification
    return Polynomial().div(Polynomial().add(p.negate(), False), 8, False)


def op_p(a, b):
    return a.multiply(b, False)


def op_s(p):
    res = Polynomial()
    for i in range(1, 8):  # ignore p[0]
        term = [0] * (i + 1)
        term[0] = 1
        term[i] = -1
        res = res.div(Polynomial(c=term).pow(p[i], 8, False), 8, False)
    return res


st = []

for i in input():
    if i == '(':
        st.append('(')
    elif i == 'B':
        st.append(Polynomial(c=[0, 1]))
    elif i == 'P' or i == 'L' or i == 'S':
        st.append(i)
    elif i == ',':
        continue
    elif i == ')':
        args = []
        while st[-1] != '(':
            args = [st[-1]] + args
            st.pop(-1)
        st.pop(-1)
        f = st.pop(-1)
        if f == 'P':
            st.append(op_p(*args))
        elif f == 'L':
            st.append(op_l(*args))
        elif f == 'S':
            st.append(op_s(*args))

print(*[st[-1][i] for i in range(7)])
