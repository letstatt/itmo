from utils.utils_linear_regression import *
from utils.graphics_common import *


# APPROX PLOTS

def basic_approx_plot(ax, points, T, SGD, MGD, GD, log=True):
    title = "Аппроксимация"
    if not log:
        title += ", нелогарифмическая шкала"
    ax.set_title(title, color='black')
    if log:
        ax.set_yscale('symlog')
        ax.set_xscale('symlog')
    legend = []
    if SGD is not None:
        ax.plot(T, [linear_combination([x], SGD[-1]) for x in T], '-', color="green", linewidth=0.3)
        legend.append("SGD")
    if MGD is not None:
        ax.plot(T, [linear_combination([x], MGD[-1]) for x in T], '-', color="yellow", linewidth=0.3)
        legend.append("MGD")
    if GD is not None:
        ax.plot(T, [linear_combination([x], GD[-1]) for x in T], '-', color="blue", linewidth=0.3)
        legend.append("GD")
    if legend:
        ax.legend(legend, labelcolor="black")
    if points:
        ax.plot([i[0][0] for i in points], [i[1] for i in points], 'o', markersize=3)


def basic_approx_plot_3d(ax3d, points, Tx, Ty, gd_points, label):
    def err_f(*args):
        return linear_combination(args, gd_points[-1])

    ax3d.set_title("Аппроксимация", color='black')
    Mx, My = np.meshgrid(Tx, Ty)
    if points:
        ax3d.scatter(
            [i[0][0] for i in points],
            [i[0][1] for i in points],
            [i[1] for i in points], 'o', s=5, cmap=plt.get_cmap('hsv'))
        ax3d.legend([label], labelcolor="black")
        errors = list()
        for i in points:
            predicted_z = err_f(*i[0])
            t = np.linspace(i[1], predicted_z, 80)
            for j in t:
                errors.append([i[0][0], i[0][1], j])
            arr = np.array(errors)
            ax3d.scatter(
                arr[:, 0],
                arr[:, 1],
                arr[:, 2], 'o', s=0.1, alpha=0.5)
    return basic_3d_plot2(ax3d, err_f, Mx, My, None, alpha=0.7)


# LINEAR REGRESSION RESULTS ANALYSIS

def basic_error_chart(ax, err_f, SGD, MGD, GD, title=None):
    labels = []
    data = []
    for name, arr in [("SGD", SGD), ("MGD", MGD), ("GD", GD)]:
        if arr is not None:
            labels.append(name)
            data.append(err_f(*arr))

    basic_bar_chart(ax, labels, data, title="Функция ошибки")


def basic_log_stats(err_f, dataset_size, batch_count, *args):
    print("Значения функции ошибки:")
    print([err_f(*p[-1]) for p in args])
    print("Результаты методов:")
    print([p[-1] for p in args])
    print("Количество итераций:")
    print([len(p) for p in args])
    print("Точек всего:", dataset_size)
    print("Точек взято в minibatch:", batch_count)


def plot_loss_epoch(ax, points_gd, func):
    size = len(points_gd)
    indexes = range(size - 11, size)
    points = []
    for i in indexes:
        points.append(func(*points_gd[i]))
    print(points)
    ax.set_xlabel('Epoch')
    ax.set_ylabel('Loss')
    ax.set_yscale('log')
    ax.grid()

    plt.plot(indexes, np.array(points), 'o-')
