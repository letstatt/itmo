from utils.config import *

def TPoly(x, c):
    sum = 0
    for i in range(len(c)):
        sum += torch.pow(x, len(c) - i - 1) * c[i]
    return sum
