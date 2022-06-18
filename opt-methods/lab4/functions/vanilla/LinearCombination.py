from utils.config import *


def f(xarray, c):
    assert len(xarray) == len(c)
    return sum(xarray * c)


def grad(xarray, c):
    assert len(xarray) == len(c)
    return np.array(xarray).reshape(-1, 1)


def hessian(xarray, c):
    assert len(xarray) == len(c)
    h = np.zeros((len(c), len(c)))
    for i in range(len(c)):
        h[i][i] = xarray[0]
    return h