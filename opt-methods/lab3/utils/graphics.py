from utils.config import *

# тут пока все функции из старой лабы. наверное, надо будет это переписать по-человечески

def basic_bar_chart(ax, x, y, title=None, log=True):
    if title:
        ax.set_title(title, color='black')
    if log:
        ax.set_yscale('log')
    ax.bar(x, y)


def basic_2d_plot(ax, f, start=-10, end=10, count=100, color='r', title=None):
    if title:
        ax.set_title(title, color='black')
    X = np.linspace(start, end, count)
    ax.plot(X, f(X), 'o-', linewidth=1, markersize=2, color=color)


def basic_3d_plot(ax, f, start=-30, stop=30, count=300, title=None):
    t = np.linspace(start, stop, count)
    X, Y = np.meshgrid(t, t)
    if title:
        ax.set_title(title, color='black')
    return ax.plot_surface(X, Y, f([X, Y]), cmap='jet')


def plot_contours(ax, points, f, title, add_labels=None, sparse=False, log=False):
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
    c = ax.contour(X, Y, f(np.array([X, Y])), levels=np.unique([f(p) for p in points]),
                   linewidths=1, colors=color_line)
    ax.plot(points[:, 0], points[:, 1], 'o-', color="blue", linewidth=1, markersize=3)
    if add_labels:
        ax.clabel(c, fontsize=4)