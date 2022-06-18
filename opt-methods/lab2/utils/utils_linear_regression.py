from utils.config import *


# y=c_1*x_1 + c_2*x_2 + c_3*x_3 + ... + c_n-1*x_n-1 + c_n
# внимание: x - вектор и c - вектор
def linear_combination(x, c):
    # мы объясняем зависимость одной переменной от взвешенных n-1 остальных
    assert len(x) + 1 == len(c)
    return sum(x[i] * c[i] for i in range(len(x))) + c[-1]


# L2 = sum((y[i] - f(*x[i])) ** 2) + alpha * sum(coeff[i] ** 2)
# alpha - параметр сглаживания, от 0.1 до 1
# внимание: points[i] = (x: vector of ints, y: int)
def error_l2(points, coeffs, alpha=1):
    return sum((y - linear_combination(x, coeffs)) ** 2 for x, y in points) + alpha * sum(i * i for i in coeffs)


class GradErrorL2:
    batch_count = None
    points = []
    pos = 0

    def __init__(self, points, alpha=1, batch_count=None):
        self.points = points * 2
        self.alpha = alpha
        self.batch_count = len(points) if not batch_count else batch_count

    def __call__(self, *coeffs):
        batch = self.points[self.pos: self.pos + self.batch_count]
        self.pos = (self.pos + self.batch_count) % len(self.points)
        return self.grad(batch, coeffs)

    def grad(self, points, coeffs):
        return [-2 * sum(
            (x[j] if j < len(x) else 1) * (y - linear_combination(x, coeffs)) for x, y in points) + 2 * self.alpha *
                coeffs[j] for j in range(len(coeffs))]
