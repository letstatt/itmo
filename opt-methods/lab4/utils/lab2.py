import numpy as np
import matplotlib.pyplot as plt

def load_points(oldStyle=False, doubleArrays=False):
    xs = []
    ys = []
    points = []
    pts2 = []
    x01_filepath = '../data/x01.txt'
    with open(x01_filepath, 'r') as f:
        n = int(f.readline().strip())
        for i in range(n):
            _, x, y = map(float, f.readline().split())
            xs.append([x])
            ys.append([y])
            points.append((np.array([y]), x))
            pts2.append((x, y))
    if (oldStyle):
        return points
    if (doubleArrays):
        return pts2
    return xs, ys

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

# grad - vector gradient
# mover - returns the next point and the new lr
# x0 - initial point (np.array)
# lr - learning rate
# stop_cond - predicate indicating whether to break

def grad_descent(grad, mover, x0, lr, stop_cond):
    points = np.array([x0])
    max_epochs = 1000
    epoch = 1
    x = x0
    prev_x = None

    while (prev_x is None or not stop_cond(prev_x, x, epoch=epoch)) and epoch <= max_epochs:
        prev_x = x
        x, lr = mover(x, lr, grad, epoch=epoch, max_epochs=max_epochs)
        points = np.append(points, [x], axis=0)
        epoch += 1
    return points

DEFAULT_CLIP_VAL = 1e9
def basic_mover(x, lr, grad, clip_val=DEFAULT_CLIP_VAL, **kwargs):
    return x - lr * np.clip(np.array(grad(*x)), -clip_val, clip_val), lr


def basic_exp_mover_factory(initial_lr, k=1, clip_val=DEFAULT_CLIP_VAL):
    return lambda x, lr, grad, epoch, max_epochs: (
        x - lr * np.clip(np.array(grad(*x)), -clip_val, clip_val),
        np.exp(np.log(initial_lr) - k * epoch / max_epochs))


def basic_stop_cond_factory(eps=1e-5):
    return lambda new_x, prev_x, **kwargs: sum(np.sqrt(np.abs(new_x - prev_x))) < eps


def basic_desired_cond_factory(desired=0, eps=1e-5):
    return lambda new_x, prev_x, **kwargs: abs(new_x - desired) < eps


def basic_gd(grad, x0, lr, mover=basic_mover, eps=1e-5, stop_cond=None):
    return grad_descent(grad, mover, x0.copy(), lr, basic_stop_cond_factory(eps) if stop_cond is None else stop_cond)

def brains_and_weight_solve(mover=None, lr=0.000007, points=None, alpha=1):
    if points is None:
        points = load_points(True)
    n = len(points)

    def f(*args):
        return error_l2(points, args, alpha)

    def grad_f(batch_count=n):
        return GradErrorL2(points, alpha, batch_count)

    x0 = np.zeros(len(points[0][0]) + 1)

    if isinstance(lr, float):
        lr = [lr, lr, lr]
        
    if mover is None:
        coeff = 17
        mover = [basic_exp_mover_factory(a, coeff, clip_val=1e5) for a in lr]
    
    if not isinstance(mover, list):
        mover = [mover, mover, mover]

    points_sgd = basic_gd(grad_f(1), x0, lr[0], mover=mover[0])
    return f(*points_sgd[-1]), points_sgd[-1]


def compate_funcs(f1, f2):
    func1, arg1, name1 = f1
    func2, arg2, name2 = f2
        
    fig = plt.figure(figsize=(5, 12))
    ax1 = fig.add_subplot(3, 1, 1)
    ax1.set_title("Аппроксимация, нелогарифмическая шкала", color='black')

    legend = []
    T = np.linspace(0, 6000)
    min1, last1 = func1(*arg1)
    min2, last2 = func2(*arg2)
    ax1.plot(T, [linear_combination([x], last1) for x in T], '-', color="green", linewidth=0.3)
    legend.append(name1)
    ax1.plot(T, [linear_combination([x], last2) for x in T], '-', color="blue", linewidth=0.3)
    legend.append(name2)
    ax1.legend(legend, labelcolor="black")
    points = load_points(True)
    ax1.plot([i[0][0] for i in points], [i[1] for i in points], 'o', markersize=3)
     
    ax2 = fig.add_subplot(3, 1, 2)
    ax2.set_title("Аппроксимация, логарифмическая шкала", color='black')  
    ax2.set_yscale('symlog')
    ax2.set_xscale('symlog')
        
    ax2.plot(T, [linear_combination([x], last1) for x in T], '-', color="green", linewidth=0.3)
    legend.append(name1)
    ax2.plot(T, [linear_combination([x], last2) for x in T], '-', color="blue", linewidth=0.3)
    legend.append(name2)
    ax2.legend(legend, labelcolor="black")
    ax2.plot([i[0][0] for i in points], [i[1] for i in points], 'o', markersize=3)
    
    ax3 = fig.add_subplot(3, 1, 3)
    ax3.bar([name1, name2], [min1, min2])
    ax3.set_title("Сравнение полученных минимумов", color='black')
    ax3.set_yscale('log')
    
    
class AdaGradMover:

    def __call__(self, x, lr, grad, **kwargs):
        gr = np.array(grad(*x))
        self.G += gr ** 2
        adjusted_grad = gr / (1e-8 + np.sqrt(self.G))
        return x - lr * adjusted_grad, lr

    def __init__(self):
        self.x0 = 0
        self.G = 0
        
class RMSPropMover:
    
    def __call__(self, x, lr, grad, **kwargs):
        gr = np.array(grad(*x))
        self.G = self.G * self.gamma + (1 - self.gamma) * (gr ** 2)
        adjusted_grad = gr / (1e-8 + np.sqrt(self.G))
        return x - lr * adjusted_grad, lr

    def __init__(self, gamma):
        self.x0 = 0
        self.G = 0
        self.gamma = gamma
        
        
class AdamMover:
    EPS = 1e-8
    m_prev = 0
    v_prev = 0
    b = 0

    def __init__(self, b1, b2):
        self.b1 = b1
        self.b2 = b2

    def __call__(self, x, lr, grad, **kwargs):
        gr = np.array(grad(*x))
        m_new = self.b1 * self.m_prev + (1 - self.b1) * gr
        self.m_prev = m_new

        v_new = self.b2 * self.v_prev + (1 - self.b2) * np.power(gr, 2)
        self.v_prev = v_new

        t = kwargs.get("epoch")
        m = m_new / (1 - np.power(self.b1, t))
        v = v_new / (1 - np.power(self.b2, t))

        return x - lr * m / (np.sqrt(v) + self.EPS), lr
