from typing import Dict
from copy import deepcopy


def limbLength(distanceMatrix, vertex):
    nodes = [i for i in range(len(distanceMatrix)) if i != vertex]
    a = nodes.pop()
    b = min(nodes, key=lambda x: (distanceMatrix[vertex][a] + distanceMatrix[vertex][x] - distanceMatrix[a][x]) / 2)
    return (distanceMatrix[vertex][a] + distanceMatrix[vertex][b] - distanceMatrix[a][b]) / 2


def get_path(g, start, end, parent = None):
    if len(g[start]) == 1:
        if start == end:
            return [(end, 0)]
        elif g[start][0][0] == parent:
            return None # dead branch
    
    for v, w in g[start]:
        if v != parent:
            path = get_path(g, v, end, start)
            if path is not None:
                return [(start, 0)] + [(j, dist + w) for j, dist in path]
    
    return None


def break_edge_and_transform(g, v, u, split_vertex, transformer):
    for j, edge in enumerate(g[v]):
        if edge[0] != u:
            continue

        w = transformer(edge[1])
        g[v][j] = (split_vertex, w)
        g[split_vertex].append((v, w))
        break


def additivePhylogeny(distanceMatrix: Dict[int, Dict[int, float]], N: int):
    n = len(distanceMatrix)
    assert n > 1

    if n == 2:
        return {
            i: [(j, dist)]
            for i, row in distanceMatrix.items() for j, dist in row.items() if i != j
        }
    
    dm = deepcopy(distanceMatrix)
    limb = limbLength(distanceMatrix, n - 1)

    for i in range(n - 1):
        dm[i][n - 1] -= limb
        dm[n - 1][i] -= limb

    found = False
    for i, k in ((i, k) for i in range(n - 1) for k in range(i + 1, n - 1)):
        if dm[i][k] == dm[i][n - 1] + dm[n - 1][k]:
            found = True
            break
    
    assert found
    x = dm[i][n - 1]

    del dm[n - 1]
    for i_ in range(n - 1):
        del dm[i_][n - 1]

    g = additivePhylogeny(dm, N)
    path = get_path(g, i, k)

    for v, dist in path:
        if dist < x:
            l = v
            path_sum = dist
        else:
            r = v
            break

    if path_sum != x:
        split_vertex = max(max(g.keys()), N - 1) + 1
        g[split_vertex] = []

        break_edge_and_transform(g, l, r, split_vertex, lambda _: x - path_sum)
        break_edge_and_transform(g, r, l, split_vertex, lambda w: w - x + path_sum)
        r = split_vertex
    
    g[r].append((n - 1, limb))
    g[n - 1] = [(r, limb)]
    return g


if __name__ == "__main__":
    n = int(input())
    dm = {}

    for i in range(n):
        dm[i] = {}
        row = map(int, input().split())
        for j, w in enumerate(row):
            dm[i][j] = w
    
    g = additivePhylogeny(dm, n)

    for i in range(n):
        for j, w in g[i]:
            print("{}->{}:{}".format(i, j, w))
    
    for i in filter(lambda k: k >= n, g.keys()):
        for j, w in g[i]:
            print("{}->{}:{}".format(i, j, w))
