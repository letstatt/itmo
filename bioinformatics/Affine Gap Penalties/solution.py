substitutions = dict()

with open("BLOSUM62.txt", "r") as f:
    chars = f.readline().strip().split()
    
    for i in range(len(chars)):
        line = f.readline().strip().split()
        fr = chars[i]
        for n, cost in enumerate(line[1:]):
            to = chars[n]
            substitutions[(fr, to)] = int(cost)

with open("input.txt", "r") as f:
    a = f.readline().strip()
    b = f.readline().strip()


def matrix(init):
    return [[init] * (len(b) + 1) for i in range(len(a) + 1)]


def update_transition(m, pm, i, j, *choices):
    best_score = -1e9
    best_parent = None
    
    assert len(choices) > 0
    for score, pi, pj, pk in choices:
        if score > best_score:
            best_score = score
            best_parent = (pi, pj, pk)
    m[i][j] = best_score
    pm[i][j] = best_parent
    

OPENING_PENALTY = 11
EXTENSION_PENALTY = 1

INS = 0
DEL = 1
MCH = 2

d = [matrix(0) for i in range(3)]
p = [matrix((-1, -1, -1)) for i in range(3)]

insertions = d[INS]
deletions = d[DEL]
matches = d[MCH]

parent_insertions = p[INS]
parent_deletions = p[DEL]
parent_matches = p[MCH]

for i in range(1, len(a) + 1):
    for j in range(1, len(b) + 1):
        update_transition(
            insertions,
            parent_insertions,
            i, j,
            (insertions[i - 1][j] - EXTENSION_PENALTY, i - 1, j, INS),
            (matches[i - 1][j] - OPENING_PENALTY, i - 1, j, MCH)
        )
        
        update_transition(
            deletions,
            parent_deletions,
            i, j,
            (deletions[i][j - 1] - EXTENSION_PENALTY, i, j - 1, DEL),
            (matches[i][j - 1] - OPENING_PENALTY, i, j - 1, MCH)
        )
        
        update_transition(
            matches,
            parent_matches,
            i, j,
            (insertions[i][j], i, j, INS),
            (deletions[i][j], i, j, DEL),
            (matches[i - 1][j - 1] + substitutions[(a[i - 1], b[j - 1])], i - 1, j - 1, MCH)
        )

ptr = (len(a), len(b), MCH)
str1 = ""
str2 = ""

while ptr[0] != -1:
    i, j, k = ptr
    pi, pj, pk = p[k][i][j]
    
    if k == INS and pk == INS:
        str1 += a[pi - 1]
        str2 += "-"
    elif k == DEL and pk == DEL:
        str1 += "-"
        str2 += b[pj - 1]
    elif k == MCH:
        if pk == INS:
            str1 += a[pi - 1]
            str2 += "-"
        if pk == DEL:
            str1 += "-"
            str2 += b[pj - 1]
        if pk == MCH:
            str1 += a[pi]
            str2 += b[pj]

    ptr = p[k][i][j]

print(d[-1][-1][-1])
print(str1[::-1])
print(str2[::-1])
