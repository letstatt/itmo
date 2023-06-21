import random

alpha = {'A': 0, 'C': 1, 'G': 2, 'T': 3}


def randomized_motif_search(dnas, k, t):
    motifs = [
        dna[random.randint(0, len(dna) - k):][:k]
        for dna in dnas
    ]
    best_motifs = motifs
    best_score = int(1e9)
    
    while True:
        p = profile(motifs)
        motifs = new_motifs(p, dnas)
        s = score(motifs)
        
        if s < best_score:
            best_motifs = motifs
            best_score = s
        else:
            return best_motifs, best_score


def profile(motifs):
    n = len(motifs[0])
    p = [[0] * n for _ in range(len(alpha))]
    
    for i in range(n):
        count = [0] * len(alpha)
        for motif in motifs:
            count[alpha[motif[i]]] += 1
        
        for char in alpha:
            p[alpha[char]][i] = (count[alpha[char]] + 1) / (len(motifs) + 4)
    
    return p


def new_motifs(p, dnas):
    k = len(p[0])
    motifs = []
    
    for i in range(len(dnas)):
        best_str = None
        best_prob = 0.0
        for j in range(len(dnas[0]) - k + 1):
            s = dnas[i][j: j + k]
            prob = 1.0
            for n, c in enumerate(s):
                prob *= p[alpha[c]][n]
            if prob > best_prob:
                best_str = s
                best_prob = prob
        motifs.append(best_str)
    return motifs


def score(motifs):
    s = 0
    for i in range(len(motifs[0])):
        count = [0] * len(alpha)
        for motif in motifs:
            count[alpha[motif[i]]] += 1
        s += sum(count) - max(count)
    
    return s


if __name__ == "__main__":
    import tqdm
    k, t = map(int, input().split())
    dnas = [input() for _ in range(t)]

    best_motifs = None
    best_score = int(1e9)

    for i in tqdm.tqdm(range(1000)):
        m, s = randomized_motif_search(dnas, k, t)
        if s < best_score:
            best_score = s
            best_motifs = m

    for i in best_motifs:
        print(i)
    #print(best_score)
