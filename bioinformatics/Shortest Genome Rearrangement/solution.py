from collections import defaultdict, deque


def int2pair(i):
    head = str(abs(i)) + ('h' if i > 0 else 't')
    tail = str(abs(i)) + ('h' if i < 0 else 't')
    return head, tail


def read_graph():
    seq = list(map(int, input()[1:-1].split()))
    adj = defaultdict(list)

    end, v = int2pair(seq[0])
    for i in seq[1:]:
        head, tail = int2pair(i)
        adj[v] = head
        v = tail
    
    adj[v] = end
    return adj

def make_non_oriented_graph(g):
    gr = dict()
    for v, u in g.items():
        gr[v] = u
        gr[u] = v
    return gr


def solver(dotted, target):
    path = [dotted.copy()]
    target_edges = sorted(set((v, u) for v, u in target.items()))
    non_oriented_target_graph = make_non_oriented_graph(target)

    while make_non_oriented_graph(path[-1]) != non_oriented_target_graph:
        g = path[-1].copy()
        complementary_edge = []

        for v, u in target_edges:
            if (v in g and g[v] == u) or (u in g and g[u] == v):
                # if an edge already exists, try next
                continue

            for x, y in list(g.items()):
                for target_vertex in (v, u):
                    if target_vertex in (x, y):
                        del g[x]
                        if target_vertex == x:
                            complementary_edge.append(y)
                        else:
                            complementary_edge.append(x)
            
            if complementary_edge:
                # preserves (v, u) edge in memory
                break
        
        assert len(complementary_edge) == 2
        g[complementary_edge[0]] = complementary_edge[1]
        g[v] = u

        path.append(g)
    
    return path


def graph_to_genome(g):
    gr = make_non_oriented_graph(g)
    q = deque(v for v in gr.keys())
    genome = []


    while q:
        seq = []
        v = q[0]

        while not seq or int(gr[v][:-1]) != abs(seq[0]):
            u_val, u_type = gr[v][:-1], gr[v][-1]
            v = u_val + ('t' if u_type == 'h' else 'h')

            seq.append(int(u_val) * (-1 if u_type == 't' else 1))
            q.remove(u_val + u_type)
            q.remove(v)

        genome.append(seq)
    return genome


if __name__ == "__main__":
    dotted = read_graph()
    target = read_graph()
    path = solver(dotted, target)

    for graph in path:
        genome = graph_to_genome(graph)
        print(''.join('(' + ' '.join(('+' if j > 0 else '') + str(j) for j in i) + ')' for i in genome))
