x = input()
input()

alphabet = input().split()
input()

states = input().split()
input()

transitions = {}
input()

for _ in states:
    fr, *probs = input().split()
    for p, to in zip(probs, states):
        transitions[(fr, to)] = float(p)
input()

emission = {}
input()

for _ in states:
    fr, *probs = input().split()
    for p, to in zip(probs, alphabet):
        emission[(fr, to)] = float(p)

V = [[emission[(state, x[0])] for state in states]]
path = [[-1] * len(states)]

for char in x[1:]:
    v_it = []
    path_it = []
    for j in states:
        best_prob = 0
        best_idx = -1
        for idx in range(len(states)):
            prob = V[-1][idx] * transitions[(states[idx], j)] * emission[(j, char)]
            if prob > best_prob:
                best_prob = prob
                best_idx = idx
        path_it.append(best_idx)
        v_it.append(best_prob)
        
    V.append(v_it)
    path.append(path_it)

max_idx = max(range(len(states)), key=lambda k: V[-1][k])
ans = ""

while max_idx != -1:
    ans += states[max_idx]
    max_idx = path[len(path) - len(ans)][max_idx]

print(ans[::-1])
