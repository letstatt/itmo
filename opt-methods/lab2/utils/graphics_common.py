from utils.config import *


# GENERAL PLOTS

def basic_bar_chart(ax, x, y, title=None, log=True):
    if title:
        ax.set_title(title, color='black')
    if log:
        ax.set_yscale('log')

    ax.bar(x, y)


def basic_scatter(ax, x, y, title=None, log=True):
    if title:
        ax.set_title(title, color='black')
    if log:
        ax.set_yscale('log')
        ax.set_xscale('log')
    ax.scatter(x, y, s=10)


def basic_scatter_3d(ax, x, y, z, title=None):
    if title:
        ax.set_title(title, color='black')
    ax.scatter(x, y, z)


def basic_3d_plot(ax, f, start=-30, stop=30, count=300, title=None):
    t = np.linspace(start, stop, count)
    X, Y = np.meshgrid(t, t)
    if title:
        ax.set_title(title, color='black')
    ax.plot_surface(X, Y, f(X, Y))


def basic_3d_plot2(ax, f, Tx, Ty, title=None, alpha=1):
    if title:
        ax.set_title(title, color='black')
    return ax.plot_surface(Tx, Ty, f(Tx, Ty), alpha=alpha)


# GRAD DESCENT RESULTS ANALYSIS

# deprecated
def plot_contours(ax, points, f, x1, x2, y1, y2, add_labels=True):
    color_line = np.zeros((len(points), 3))
    color_line[:, 1:] = 0.2
    color_line[:, 0] = [i ** 3 for i in np.linspace(0, 1, len(points))]
    t1 = np.linspace(x1, x2, 100)
    t2 = np.linspace(y1, y2, 100)
    X, Y = np.meshgrid(t1, t2)
    ax.plot(points[:, 0], points[:, 1], 'o-', linewidth=1, markersize=2)
    c = ax.contour(X, Y, f(X, Y), levels=sorted([f(*p) for p in points]),
                   linewidths=1, colors=color_line)
    if add_labels:
        ax.clabel(c, fontsize=4)


def plot_contours_new(ax, points, f, descent_type, add_labels=None, sparse=False, log=False):
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
    tmp = []
    
    ax.set_title("Траектория " + descent_type, color='black')
    if log:
        ax.set_yscale("symlog")
        ax.set_xscale("symlog")
    if sparse:
        for i in np.linspace(0, len(points), 20, dtype=int, endpoint=False):
            tmp.append(points[i])
        tmp = np.array(tmp)
    else:
        tmp = points
    color_line = np.zeros((len(tmp), 3))
    color_line[:, 1:] = 0.2
    color_line[:, 0] = [i ** 3 for i in np.linspace(0, 1, len(tmp))]
    t1 = np.linspace(x1, x2, 100)
    t2 = np.linspace(y1, y2, 100)
    X, Y = np.meshgrid(t1, t2)
    c = ax.contour(X, Y, f(X, Y), levels=sorted([f(*p) for p in tmp]),
                   linewidths=1, colors=color_line)
    ax.plot(tmp[:, 0], tmp[:, 1], 'o-', color="blue", linewidth=1, markersize=3)
    if add_labels:
        ax.clabel(c, fontsize=4)

