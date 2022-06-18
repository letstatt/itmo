from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

import matplotlib.pyplot as plt
import numpy as np

plt.style.use('ggplot')
plt.rc('figure', dpi=200, figsize=(5, 4))  # (w, h)
plt.rc('font', size=5)
plt.rc('axes', labelsize=7)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.rc('legend', fontsize=5)
plt.rc('figure', titlesize=12)
plt.rc('axes', grid=False)

RNG = np.random.default_rng(1337)
EPS = 1e-4

local_env = True
