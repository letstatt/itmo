from utils.config import *

# общий код градиентного спуска

def XStopCondFactory(eps=EPS):
    return lambda prev_x, x, **kwargs: sum(np.sqrt(np.abs(x - prev_x))) < eps

def DontStop():
    return lambda **kwargs: False

def regression(mover, x0, stop_cond=XStopCondFactory(), max_epochs=1000):
    assert type(x0) == np.ndarray, "x0 point should have np.ndarray type"

    points = np.array([x0.copy()])
    epoch = 1
    x = x0.copy()
    prev_x = None

    while (prev_x is None or not stop_cond(prev_x=prev_x, x=x, epoch=epoch)) and epoch <= max_epochs:
        prev_x = x.copy()
        x = mover(x, epoch=epoch, max_epochs=max_epochs)
        points = np.concatenate([points, [x.copy()]])
        epoch += 1
    return points

from functions.vanilla.LeastSquaresMethod import LeastSquaresMethod
import functions.vanilla.Polynomial as Poly
import functions.vanilla.LinearCombination as LinearCombination
import functions.vanilla.Rozenbrock as Rozenbrock

# код метода гаусс-ньютона.

# здесь и далее тип point задан так: p = [y, x_1, x_2, ... x_n]
# все функции градиента должны возвращать тип np.ndarray и причем как столбец!
# все иксы тем не менее хотим ради удобства передавать в муверы/regression как вектор-строку.
# мы должны учитывать что входящий икс всегда вектор-строка и как-то внутри это конвертить.

class GaussNewtonMover:
    function = None
    lr = None

    def __init__(self, function, lr):
        assert isinstance(function, LeastSquaresMethod),\
            "ахтунг, гаусс-ньютон умеет только в решение задачи минимизации суммы квадратов"
        self.function = function
        self.lr = lr
    
    def get_error(self, c):
        return self.function.get_error(c)

    def __call__(self, x, **kwargs):
        g = self.function.grad(x)
        h = self.function.hessian(x)
        p = np.matmul(np.linalg.inv(h), g)
        return (x.reshape(-1, 1) - self.lr * p).T[0] # внимание тут дважды транспонирование икса
    
    def get_stop_cond(self, eps=1e-8):
        return lambda x, **kwargs: self.get_error(x) < eps


# код метода Powell's DogLeg (или как говорят, метод собачьей лапки Пауэла)

class DogLegMover:
    function = None
    max_tr = None
    tr = None
    eta = None

    def __init__(self, function, initial_tr=1.0, max_tr=100.0, eta=0.15):
        assert initial_tr <= max_tr, "initial_tr must be <= than max_tr"
        self.function = function
        self.tr = initial_tr
        self.max_tr = max_tr
        self.eta = eta

    def make_step(self, g, h):
        # steppest descent direction
        pU = -(np.matmul(g.T, g) / np.matmul(g.T, np.dot(h, g))) * g
        pU_norm = np.linalg.norm(pU)

        # case 1
        if pU_norm >= self.tr:
            return self.tr * pU / pU_norm
        
        # direction to global minimum of quadratic model
        pH = -np.matmul(np.linalg.inv(h), g)
        pH_norm = np.linalg.norm(pH)

        # case 2
        if pH_norm <= self.tr:
            return pH
        
        # case 3
        A = pU
        B = pH - pU
        discriminant = np.matmul(A.T, B)**2 - np.matmul(B.T, B) * (np.matmul(A.T, A) - self.tr**2)
        tau = (-np.matmul(A.T, B) + np.sqrt(discriminant)) / np.matmul(B.T, B)

        return A + tau * B

    def __call__(self, x, **kwargs):
        g = self.function.grad(x)
        h = self.function.hessian(x)
        self.prev_g = g # store gradient to use in stop cond

        # Get direction
        p = self.make_step(g, h)

        # Actual reduction
        act_red = self.function.f(x) - self.function.f((x + p.T)[0])
        
        # Predicted reduction
        pred_red = -(np.matmul(g.T, p) + 0.5 * np.matmul(p.T, np.dot(h, p)))
        
        # Calc rho
        if pred_red == 0.0:
            rho = 1e99
        else:
            rho = act_red / pred_red
        
        # Change tr
        if rho < 0.25:
            self.tr = 0.25 * self.tr
        elif rho > 0.75 and np.abs(np.linalg.norm(p) - self.tr) < 1e-9:
            self.tr = min(2.0 * self.tr, self.max_tr)

        # Choose the position for the next iteration.
        if rho > self.eta:
            return (x + p.T)[0]
        else:
            return x
    
    def get_stop_cond(self, eps=EPS):
        return lambda **kwargs: self.prev_g is not None and np.linalg.norm(self.prev_g) < eps


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


import traceback

# код для метода BFGS

class BFGSMover:
    function = None
    hinv = None
    lr_policy = None
    c1 = None
    c2 = None
    beta = None
    prev_g = None
    debug = False

    def __init__(self, function, lr_policy, beta=1, hinv=None, c1=1e-4, c2=0.9, debug=False):
        self.function = function
        self.lr_policy = lr_policy
        self.c1 = c1
        self.c2 = c2
        self.beta = beta
        self.hinv = hinv
        self.debug = debug
    
    def setup(self, dim):
        self.hinv = self.beta * np.eye(dim)
    
    def update_hinv(self, s, y):
        p = 1 / np.matmul(y.T, s)
        a = p * np.matmul(s, y.T)
        b = p * np.matmul(y, s.T)
        c = p * np.matmul(s, s.T)
        i = np.eye(len(s))
        self.hinv = np.matmul((i - a), np.matmul(self.hinv, (i - b))) + c
    
    def __call__(self, x, **kwargs):
        if self.hinv is None:
            self.setup(len(x))
        
        g = self.function.grad(x)
        p = -np.matmul(self.hinv, g)
        try:
            x_new = self.lr_policy(self.function.f, self.function.grad, x, p, self.c1, self.c2)
        except Exception as e:
            if self.debug:
                print(traceback.format_exc()) #  не смогли обновиться
            self.prev_g = np.zeros(len(g))
            return x
        
        y = self.function.grad(x_new) - g
        s = (x_new - x).reshape(-1, 1)
        self.prev_g = g

        self.update_hinv(s, y)
        return x_new

    def get_stop_cond(self, eps=EPS):
        return lambda **kwargs: self.prev_g is not None and np.linalg.norm(self.prev_g) < eps
