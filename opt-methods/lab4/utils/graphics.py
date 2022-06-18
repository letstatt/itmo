from utils.config import *

def basic_bar_chart(ax, x, y, title=None, log=True, color='red'):
    if title:
        ax.set_title(title, color='#293133')
    if log:
        ax.set_yscale('log')
    ax.bar(x, y, color=color)


def basic_2d_plot(ax, f, start=-10, end=10, count=100, color='r', title=None):
    if title:
        ax.set_title(title, color='#293133')
    X = np.linspace(start, end, count)
    ax.plot(X, f(X), 'o-', linewidth=1, markersize=2, color=color)


def basic_3d_plot(ax, f, start=-30, stop=30, count=300, title=None):
    t = np.linspace(start, stop, count)
    X, Y = np.meshgrid(t, t)
    if title:
        ax.set_title(title, color='#293133')
    return ax.plot_surface(X, Y, f(X, Y), cmap='jet')


def plot_contours(ax, points, f, title, add_labels=None, log=False):
    x1 = min(points[:, 0])
    x2 = max(points[:, 0])
    y1 = min(points[:, 1])
    y2 = max(points[:, 1])
    dx = abs(x1 - x2) / 5
    dy = abs(y1 - y2) / 5
    x1 -= dx
    x2 += dx
    y1 -= dy
    y2 += dy
    if add_labels is None:
        add_labels = (len(points) < 20)
    
    ax.set_title("Траектория " + title, color='black')
    if log:
        ax.set_yscale("symlog")
        ax.set_xscale("symlog")
    color_line = np.zeros((len(points), 3))
    color_line[:, 1:] = 0.2
    color_line[:, 0] = [i ** 3 for i in np.linspace(0, 1, len(points))]
    t1 = np.linspace(x1, x2, 100)
    t2 = np.linspace(y1, y2, 100)
    X, Y = np.meshgrid(t1, t2)
    c = ax.contour(X, Y, f(X, Y), levels=np.unique([f(p) for p in points]),
                   linewidths=1, colors=color_line)
    ax.plot(points[:, 0], points[:, 1], 'o-', color="blue", linewidth=1, markersize=3)
    if add_labels:
        ax.clabel(c, fontsize=4)

def minimize_print_status(resultObj):
    print("Успешно: {}".format(resultObj.success))
    print("Значение функции: {}".format(resultObj.fun))
    print("Точка минимума: {}".format(resultObj.x))
    print("Количество итераций метода: {}".format(resultObj.nit))
    print("Количество вызовов функции: {}".format(resultObj.nfev))
    print("Посчитано якобианов: {}".format(resultObj.njev))
    print("Комментарий: {}".format(resultObj.message))


'''obj1 - without pytorch, obj2 - with pytorch autograd'''

def minimize_comp(obj1, obj2):
    fig = plt.figure(figsize=(6, 3))
    ax1 = fig.add_subplot(1, 2, 1)
    ax2 = fig.add_subplot(1, 2, 2)
    print("Результат с PyTorch Autograd")
    minimize_print_status(obj2)

    func_titles = ['func #1', 'func #2']
    func_nums = [obj1.fun, obj2.fun]
    basic_bar_chart(ax1, func_titles, func_nums, title="Optimality (less - better)", color='#9966CC')

    jac_titles = ['jac #1', 'jac #2']
    jac_nums = [np.linalg.norm(obj1.jac), np.linalg.norm(obj2.jac)]
    basic_bar_chart(ax1, jac_titles, jac_nums, color='#6A5ACD')

    nfev_titles = ['nfev #1', 'nfev #2']
    nfev_nums = [obj1.nfev + 2*obj1.njev, obj2.nfev + obj2.njev]
    basic_bar_chart(ax2, nfev_titles, nfev_nums, title="Evaluations (less - better)", log=False, color='#44944A')

    evals_titles = ['iters #1', 'iters #2']
    evals_nums = [obj1.nit, obj2.nit]
    basic_bar_chart(ax2, evals_titles, evals_nums, log=False, color='#9F2B68')
    plt.show()


def least_squares_print_status(resultObj):
    print("Успешно: {}".format(resultObj.success))
    print("Оптимальность: {}".format(resultObj.optimality))
    print("Функция ошибки: {}".format(resultObj.cost))
    print("Подобранные параметры: {}".format(resultObj.x))
    print("Количество вызовов функции: {}".format(resultObj.nfev))
    print("Посчитано якобианов: {}".format(resultObj.njev))
    print("Код возврата: {}".format(resultObj.status))


def function_plot(ax, f, start=-50, end=50, count=100, color='orange'):
    X = np.linspace(start, end, count)
    ax.plot(X, f(X), 'o-', linewidth=1.5, markersize=0.35, color=color, zorder=1)


def predicted_plot(ax, predicted, f, points, title=None):
    X = list(p[1] for p in points)
    Y = list(p[0] for p in points)
    x_min = min(X)
    x_max = max(X)
    dx = abs(x_min - x_max) / 5
    function_plot(ax, lambda X: f(torch.tensor(X), predicted), start=x_min-dx, end=x_max+dx)
    ax.scatter(X, Y, 3, color="blue", zorder=2)
    if title is not None:
        ax.set_title(title, color='#293133')


def least_squares_result(resultObj, f, points, log=False, ax=None, title="Predicted"):
    silent_mode = (ax is not None)
    if not silent_mode:
        fig = plt.figure(figsize=(2, 2))
        ax = fig.add_subplot(1, 1, 1)
        if log:
            ax.set_yscale("symlog")
        least_squares_print_status(resultObj)
    predicted_plot(ax, resultObj.x, f, points, title=title)
    if not silent_mode:
        plt.show()


'''obj1 - without pytorch, obj2 - with pytorch autograd'''

def least_squares_comp(obj1, obj2, f, points, log=False):
    fig = plt.figure(figsize=(6, 6))
    ax1 = fig.add_subplot(2, 2, 1)
    ax2 = fig.add_subplot(2, 2, 2)
    ax3 = fig.add_subplot(2, 2, 3)
    ax4 = fig.add_subplot(2, 2, 4)
    if log:
            ax1.set_yscale("symlog")
            ax2.set_yscale("symlog")
    print("Наиболее успешный результат")
    least_squares_print_status(obj1 if obj1.optimality < obj2.optimality else obj2)
    least_squares_result(obj1, f, points, ax=ax1, title="Predicted #1")
    least_squares_result(obj2, f, points, ax=ax2, title="Predicted #2")

    cost_titles = ['cost #1', 'cost #2']
    cost_nums = [obj1.cost, obj2.cost]
    basic_bar_chart(ax3, cost_titles, cost_nums, title="Optimality (less - better)", color='#9966CC')

    opt_titles = ['grad #1', 'grad #2']
    opt_nums = [obj1.optimality, obj2.optimality]
    basic_bar_chart(ax3, opt_titles, opt_nums, color='#6A5ACD')

    nfev_titles = ['nfev #1', 'nfev #2']
    nfev_nums = [obj1.nfev + 2*obj1.njev, obj2.nfev + obj2.njev]
    basic_bar_chart(ax4, nfev_titles, nfev_nums, title="Evaluations (less - better)", log=False, color='#44944A')

    evals_titles = ['iters #1', 'iters #2']
    evals_nums = [obj1.nfev, obj2.nfev]
    basic_bar_chart(ax4, evals_titles, evals_nums, log=False, color='#9F2B68')
    plt.show()


def least_squares_methods_comp(points, vanilla_model, t_model, gn_lr, initial_tr=100, eta=1e-10):
    import utils
    from functions.torch.LeastSquaresMethod import TLeastSquares
    point_zero = np.zeros(len(points[0]) - 1)
    m = utils.lab3.LeastSquaresMethod(points, vanilla_model)
    gn = utils.lab3.GaussNewtonMover(m, gn_lr)
    stop_cond = lambda x, **kwargs: gn.function.get_error(x) < 1e-8 or np.abs(np.linalg.norm(kwargs['prev_x'] - x)) < 1e-8
    regr = utils.lab3.regression(gn, point_zero, stop_cond)
    print("--- Gauss-Newton ---")
    print("result", regr[-1])
    print("iterations", len(regr) - 1)
    print("error", gn.function.f(regr[-1]))
    m = TLeastSquares(t_model, points)
    p = least_squares(m, point_zero, jac=m.jacobian, method="lm") # nice jacockbian
    print("--- LM ---")
    least_squares_print_status(p)
    m = utils.lab3.LeastSquaresMethod(points, vanilla_model)
    dl = utils.lab3.DogLegMover(m, initial_tr=initial_tr, eta=eta)
    stop_cond = lambda x, **kwargs: dl.function.get_error(x) < 1e-8 or np.abs(np.linalg.norm(kwargs['prev_x'] - x)) < 1e-8
    regr = utils.lab3.regression(dl, point_zero, stop_cond)
    print("--- Dogleg ---")
    print("result", regr[-1])
    print("iterations", len(regr) - 1)
    print("error", dl.function.f(regr[-1]))
