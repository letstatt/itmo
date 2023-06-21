from collections import defaultdict
import sys


def FindEulerPath(g, v, path):
    while len(g[v]):
        u = g[v].pop()
        FindEulerPath(g, u, path)
    path.append(v)


with open("input.txt", "r") as f:
    sys.setrecursionlimit(1500)

    k, d = map(int, f.readline().split())
    pairs = [composition.strip().split("|") for composition in f.readlines()]
    
    graph = defaultdict(list)
    degree_in = defaultdict(int)
    degree_out = defaultdict(int)

    for s1, s2 in pairs:
        # shift by one
        from_ = (s1[:-1], s2[:-1])
        to_ = (s1[1:], s2[1:])

        graph[from_].append(to_)
        degree_in[to_] += 1
        degree_out[from_] += 1
    
    start = None

    for v in set(list(degree_in.keys()) + list(degree_out.keys())):
        if degree_in[v] < degree_out[v]:
            start = v
            break
    
    assert start is not None

    path = []
    FindEulerPath(graph, start, path)
    path.reverse()

    left = [start[0]] + [x[k - 2:] for x, _ in path[1:]]
    right = [start[1]] + [y[k - 2:] for _, y in path[1:]]

    print(''.join(left) + ''.join(right)[-k - d:])
