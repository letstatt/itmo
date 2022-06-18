from utils.config import *


def f(xarray, c):
    assert len(xarray) == 1, "accepts one variable only"
    return sum(pow(xarray[0], len(c) - i - 1) * c[i] for i in range(len(c)))


def grad(xarray, c):
    assert len(xarray) == 1, "accepts one variable only"
    return np.array([pow(xarray[0], len(c) - i - 1) for i in range(len(c))]).reshape(-1, 1)


def hessian(xarray, c):
    assert len(xarray) == 1, "accepts one variable only"
    h = np.zeros((len(c), len(c)))
    for i in range(len(c)):
        h[i][i] = pow(xarray[0], len(c) - i - 1)
    return h