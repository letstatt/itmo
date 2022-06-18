import matplotlib.pyplot as plt
import numpy as np

plt.style.use('ggplot')
plt.rc('figure', dpi=200, figsize=(5, 5))  # (w, h)
plt.rc('font', size=5)
plt.rc('axes', labelsize=7)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.rc('legend', fontsize=5)
plt.rc('figure', titlesize=12)

RNG = np.random.default_rng(1337)
DEFAULT_CLIP_VAL = 1e9
EPS = 1e-4

local_env = True

x01_filepath = '/content/sample_data/x01.txt'
x06_filepath = '/content/sample_data/x06.txt'

if local_env:
    x01_filepath = '../data/x01.txt'
    x06_filepath = '../data/x06.txt'

