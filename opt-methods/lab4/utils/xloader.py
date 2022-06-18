from utils.config import *

def points(filename):
    points = []
    with open('../data/' + filename, 'r') as f:
        for line in f.readlines():
            if len(line) == 0 or line[0] == '#':
                continue
            _, *data = map(float, line.split())
            point = [data[-1]] + data[0: len(data) - 1]
            points.append(torch.tensor(point))
    return points
