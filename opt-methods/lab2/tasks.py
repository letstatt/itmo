from utils.config import *
from utils.graphics_common import *


# TASK 1a ----------------------------------------------------------------------
# ПРОСТАЯ ФУНКЦИЯ


def simple_function(basic_gd):
    def f(x, y):
        return 0.01 * x ** 2 + 0.05 * y ** 2

    def grad_f(x, y):
        return 0.02 * x, 0.1 * y

    x0 = np.array([10, 10])
    lr = 14 # поверхность пологая ближе к минимуму

    points_gd = basic_gd(grad_f, x0, lr)

    fig = plt.figure(figsize=(5, 8))
    ax1 = fig.add_subplot(2, 1, 1, projection='3d')
    ax2 = fig.add_subplot(2, 1, 2)

    basic_3d_plot(ax1, f, title="Поверхность исследуемой функции")
    plot_contours_new(ax2, points_gd, f, "GD", add_labels=True)
    plt.show()

    print("Полученный минимум:", f(*points_gd[-1]))
    print("Количество итераций:", len(points_gd))


# TASK 1.2 ---------------------------------------------------------------------
# МОЗГИ И МАССА ТЕЛА


def brains_and_weight_points():
    points = []
    with open(x01_filepath, 'r') as f:
        n = int(f.readline().strip())
        for i in range(n):
            _, x, y = map(float, f.readline().split())
            points.append((np.array([y]), x))
    # внимание: points[i] = (x: vector of ints, y: int)
    # в данном случае: points[i] = (x: vector of one int, y: int)
    return points


def brains_and_weight_plot():
    points = brains_and_weight_points()
    x = [i[0][0] for i in points]
    y = [i[1] for i in points]

    _, ax = plt.subplots()
    basic_scatter(ax, x, y, "Зависимость массы мозга от массы тела млекопитающих")
    plt.show()


# TASK 1.3 ---------------------------------------------------------------------
# ДЛИНА РЫБЫ И ТЕМПЕРЕТУРА


def fish_points():
    points = []
    with open(x06_filepath, 'r') as f:
        n = int(f.readline().strip())
        for i in range(n):
            _, age, temp, length = map(float, f.readline().split())
            points.append((np.array([age, temp]), length))
    # внимание: points[i] = (x: vector of ints, y: int)
    # в данном случае: points[i] = (x: vector of two ints, y: int)
    return points


def fish_plot():
    points = fish_points()
    x = [i[0][0] for i in points]
    y = [i[0][1] for i in points]
    z = [i[1] for i in points]

    _, ax = plt.subplots(subplot_kw=dict(projection="3d"))
    basic_scatter_3d(ax, x, y, z, "Зависимость длины рыбы от ее возраста и температуры воды")
    plt.show()

