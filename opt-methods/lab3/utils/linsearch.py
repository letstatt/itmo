from utils.config import *


# из первой лабы с любовью
# условия Вольфе и линейный поиск

def first_cond(x_k, p_k, alpha, f, c1=0.0001):
    return f((x_k.reshape(-1, 1) + alpha * p_k).T[0]) <= f(x_k) + c1 * alpha * np.matmul(p_k.T, p_k)


def second_cond(x_k, p_k, alpha, grad, c2=0.9):
    return np.matmul(grad((x_k.reshape(-1, 1) + alpha * p_k).T[0]).T[0], p_k) >= c2 * np.matmul(p_k.T, p_k)


class Linsearch:
    min_lr = None
    max_lr = None

    def __init__(self, min_lr=1e-15, max_lr=2, step=0.1):
        self.min_lr = min_lr
        self.max_lr = max_lr
        self.step = step

    def __call__(self, f, grad, x, direction, c1, c2):
        l = self.min_lr # min learning rate
        r = self.max_lr # max learning rate
        optimum = None
        f_opt = None

        while l <= r:
            if not first_cond(x, direction, l, f, c1): # big steps causes going away from minimum
                l += self.step
            elif not second_cond(x, direction, l, grad, c2): # small steps limits moving
                l += self.step
            else:
                x = (x.reshape(-1, 1) + l * direction).T[0]
                f_x = f(x)

                if f_opt == None or f_x <= f_opt:
                    f_opt = f_x
                    optimum = x
                l += self.step

        if optimum is not None:
            return optimum
        else:
            raise Exception("search failed :(")






# incompatible functions to lab3_high_order_methods
# used in Task3

def first_wolfe_condition(f, x, d, a, b1, grad):
    return f(x) + a * b1 * np.dot(grad(x), d) >= f(x + a * d)


def second_wolfe_condition(x, d, a, b2, grad):
    return np.dot(grad(x + a * d), d) >= b2 * np.dot(grad(x), d)


def line_search(f, g, x, d, c1=1e-4, c2=0.9):
    eps = 1e-12
    l = 0
    r = 1
    d = d.T[0]
    grad = lambda x: g(x).T[0]
    if f(x + r * d) < f(x + l * d):
        while f(x + r * d) > f(x + 2 * r * d):
            r *= 2
    else:
        while f(x + r * d) >= f(x + l * d) and f(x + r * d) > f(x + r / 2 * d):
            r /= 2

    while r - l > eps:
        a = (l + r) / 2
        pass_first = first_wolfe_condition(f, x, d, a, c1, grad)
        if not pass_first:
            r = (l + r) / 2
            a = (l + r) / 2
        pass_second = second_wolfe_condition(x, d, a, c2, grad)
        if not pass_second:
            l = a
        if pass_first and pass_second:
            return x + a *d

    raise Exception("search failed :(")
