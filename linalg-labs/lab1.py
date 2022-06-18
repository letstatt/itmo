
def read_matrix(f):
    n, m = map(int, f.readline().split())
    d = list(map(float, f.readline().split()))
    q = Matrix(n, m)
    
    for i in range(n * m):
        q.set(i // m, i % m, d[i])
    return q


def main():
    with open('input.txt', 'r') as f:
        a, b = map(float, f.readline().split())
        A = read_matrix(f)
        B = read_matrix(f)
        C = read_matrix(f)
        D = read_matrix(f)
        F = read_matrix(f)
    
    try:
        X = C * (a * A + b * B.transpose()).transpose() * D - F
    except:
        X = None
    
    with open('output.txt', 'w') as f:
        if X is None:
            f.write('0')
        else:
            f.write('1\n')
            f.write(str(X.height()))
            f.write(' ' + str(X.width())+'\n')
            f.write(str(X))
    

# My matrix lib
# Types were erased due to using python 3.5 in ejudge

class Matrix:
        
    def __init__(self, height = -1, width = -1, m = None, arr = None):
        if height > 0 or width > 0:
            self.__m = [[0] * width for i in range(height)]
        elif m is not None:
            self.__m = m.raw_matrix()
        elif arr is not None:
            self.__m = [arr[i].copy() for i in range(len(arr))]
        else:
            raise ValueError('Pattern matching failed')
        
        self.__recalc()
        if m is not None:
            self.__det = m.det()
    
    def __recalc(self):
        self.__height = len(self.__m)
        self.__width = len(self.__m[0])
        self.__det = None

        for i in self.__m:
            if len(i) != self.__width:
                return ValueError('Matrix must be rectangle')
        
    # Basic methods
    
    def height(self):
        return self.__height
    
    def width(self):
        return self.__width
    
    def get_row(self, i):
        return self.__m[i].copy()
    
    def get_column(self, j):
        col = [self.height()]
        for i in range(self.height()):
            col[i] = self.__m[i][j]
        return col
    
    def get(self, i, j):
        return self.__m[i][j]
    
    def set(self, i, j, k):
        self.__m[i][j] = k
        self.__det = None  # important
    
    def det(self):
        if self.__det is None:
            self.__det = MatrixUtils.det(self)
        return self.__det
    
    def transpose(self):
        return MatrixUtils.transpose(self)
        
    def raw_matrix(self):
        return [i.copy() for i in self.__m]
    
    def copy(self):
        return Matrix(m=self)
    
    # System methods
    
    def __eq__(self, b):
        if self.height() != b.height() or self.width() != b.width():
            return False
        for i in range(self.height()):
            for j in range(self.width()):
                if self.get(i, j) != b.get(i, j):
                    return False
        return True
    
    def __ne__(self, b):
        return not self.__eq__(b)
    
    def __bool__(self):
        for i in range(self.height()):
            for j in range(self.width()):
                if self.get(i, j) != 0:
                    return True
        return False
    
    def __str__(self):
        s = ""
        for i in range(self.height()):
            s += ' '.join(map(str, self.__m[i])) + "\n"
        return s
    
    # Arithmetic methods
    
    def __add__(self, b):
        if self.height() != b.height() or self.width() != b.width():
            raise TypeError('Both matrices must have equal sizes')

        a = [[self.get(i, j) + b.get(i, j) for j in range(self.width())] for i in range(self.height())]
        return Matrix(arr=a)

    def __sub__(self, b):
        if self.height() != b.height() or self.width() != b.width():
            raise TypeError('Both matrices must have equal sizes')

        a = [[self.get(i, j) - b.get(i, j) for j in range(self.width())] for i in range(self.height())]
        return Matrix(arr=a)
    
    def __mul__(self, b):
        if type(b) == Matrix:
            if self.width() != b.height():
                raise TypeError('Matrices must be consistent')

            def acc(i, j):
                res = 0
                for k in range(self.width()):
                    res += self.get(i, k) * b.get(k, j)
                return res

            a = [[acc(i, j) for j in range(b.width())] for i in range(self.height())]
        else:
            a = [[b * j for j in i] for i in self.__m]

        return Matrix(arr=a)

    def __rmul__(self, b):
        return self.__mul__(b)
    
    def __pow__(self, b):
        if self.height() != self.width():
            raise TypeError('Power is defined for square matrices only')
        if b <= 0:
            raise ArithmeticError('Power must be greater than 0')

        return self**(b-1) * self if b > 1 else self  # trivial for the beginning


class MatrixUtils:

    @staticmethod
    def det(m):
        if m.height() != m.width():
            raise TypeError('Determinant is defined for square matrices only')
        
        return MatrixUtils.determinant_decomposition(m.raw_matrix())
    
    @staticmethod
    def determinant_decomposition(m):
        i1, i2 = 0, len(m)
        j1, j2 = 0, len(m[0])
        res = 0

        if i2 - i1 == 1:
            return m[i1][j1]
        if i2 - i1 == 2:
            return m[i1][j1] * m[i1 + 1][j1 + 1] - m[i1][j1 + 1] * m[i1 + 1][j1]

        for i in range(i1, i2):
            minor = [m[j][j1+1:j2] for j in range(i1, i2) if i != j]
            res += (1 if (j1 + i) % 2 == 0 else -1) * m[i][0] * MatrixUtils.determinant_decomposition(minor)
        return res
    
    @staticmethod
    def transpose(m):
        a = [[m.get(j, i) for j in range(m.height())] for i in range(m.width())]
        return Matrix(arr=a)


if __name__ == '__main__':
    main()