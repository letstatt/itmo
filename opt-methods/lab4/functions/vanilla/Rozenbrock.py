from utils.config import *

def f(xarray):
    x, y = xarray
    return (1 - x)**2 + 100 * (y - x**2)**2


def grad(xarray):
    x, y = xarray
    return np.array([-400 * (y - x**2) * x - 2 + 2 * x, 200 * y - 200 * x**2], dtype=np.float64).reshape(-1, 1)


def hessian(xarray):
    x, y = xarray
    return np.array([[1200 * x**2 - 400 * y + 2, -400 * x], [-400 * x, 200]], dtype=np.float64)