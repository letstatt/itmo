from utils.config import *

def TLinearCombination(x, c):
    assert len(x) == len(c)
    sum = torch.tensor([.0], dtype=torch.float64)
    for i in range(len(c)):
        sum += x[i] * c[i]
    return sum
