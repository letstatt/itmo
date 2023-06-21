#  https://en.wikipedia.org/wiki/Baum%E2%80%93Welch_algorithm


def read_matrix(rows):
    columns = input().strip().split()
    m = {}
    for row in rows:
        _, *values = input().split()  # ignore row label due to bug
        assert len(values) == len(columns)

        m[row] = {}
        for j in range(len(values)):
            m[row][columns[j]] = float(values[j])
    
    return m


def print_matrix(m, rows, columns):
    print('\t' + '\t'.join(columns))
    for r in rows:
        print(r, *(round(i, 3) for i in m[r].values()), sep='\t')


n = int(input())
input()  # skip line

emitted = input()
input()  # skip line

e_alphabet = input().split()
input()  # skip line

a_alphabet = input().split()
input()  # skip line

A = read_matrix(a_alphabet)
input()  # skip line

B = read_matrix(a_alphabet)
pi = [1/len(a_alphabet) for _ in range(len(a_alphabet))]


def estimate_alpha(A, B, pi, a_alphabet, emitted):
    alpha = [[0] * len(a_alphabet) for _ in range(len(emitted))]  # alpha[t][j]
    alpha[0] = [pi[j] * B[a_alphabet[j]][emitted[0]] for j in range(len(a_alphabet))]

    for t in range(1, len(emitted)):
        for j in range(len(a_alphabet)):
            for i in range(len(a_alphabet)):
                alpha[t][j] += alpha[t - 1][i] * A[a_alphabet[i]][a_alphabet[j]] * B[a_alphabet[j]][emitted[t]]
            
    return alpha


def estimate_beta(A, B, a_alphabet, emitted):
    beta = [[0] * len(a_alphabet) for _ in range(len(emitted))]  # beta[t][j]
    beta[-1] = [1] * len(a_alphabet)

    for t in range(len(emitted) - 2, -1, -1):
        for i in range(len(a_alphabet)):
            for j in range(len(a_alphabet)):
                beta[t][i] += A[a_alphabet[i]][a_alphabet[j]] * B[a_alphabet[j]][emitted[t + 1]] * beta[t + 1][j]

    return beta


def estimate_gamma(alpha, beta, a_alphabet, emitted):
    gamma = [[0] * len(a_alphabet) for _ in range(len(emitted))]  # gamma[t][i]

    for t in range(len(emitted)):
        for i in range(len(a_alphabet)):
            gamma[t][i] = alpha[t][i] * beta[t][i]
            s = 0
            for j in range(len(a_alphabet)):
                s += alpha[t][j] * beta[t][j]
            gamma[t][i] /= s
    
    return gamma


def estimate_eta(alpha, beta, A, B, a_alphabet, emitted):
    eta = [[[0] * len(a_alphabet) for _ in range(len(a_alphabet))] for _ in range(len(emitted) - 1)]  # eta[t][i][j]

    for t in range(len(emitted) - 1):
        for i in range(len(a_alphabet)):
            for j in range(len(a_alphabet)):
                eta[t][i][j] = alpha[t][i] * A[a_alphabet[i]][a_alphabet[j]] * beta[t + 1][j] * B[a_alphabet[j]][emitted[t + 1]]
                s = 0

                for k in range(len(a_alphabet)):
                    for w in range(len(a_alphabet)):
                        s += alpha[t][k] * A[a_alphabet[k]][a_alphabet[w]] * beta[t + 1][w] * B[a_alphabet[w]][emitted[t + 1]]
                eta[t][i][j] /= s
    return eta


def estimate_pi(gamma):
    return gamma[1]


def estimate_a(gamma, eta, a_alphabet, emitted):
    a = {}

    for i in range(len(a_alphabet)):
        a[a_alphabet[i]] = {}
        for j in range(len(a_alphabet)):
            e = 0
            g = 0

            for t in range(len(emitted) - 1):
                e += eta[t][i][j]
                g += gamma[t][i]
            
            a[a_alphabet[i]][a_alphabet[j]] = e / g
    return a


def estimate_b(gamma, a_alphabet, e_alphabet, emitted):
    b = {}

    for i in range(len(a_alphabet)):
        b[a_alphabet[i]] = {}
        for j in range(len(e_alphabet)):
            g1 = 0
            g2 = 0

            for t in range(len(emitted)):
                g1 += gamma[t][i] if emitted[t] == e_alphabet[j] else 0
                g2 += gamma[t][i]
            
            b[a_alphabet[i]][e_alphabet[j]] = g1 / g2
    return b


for _ in range(n):
    alpha = estimate_alpha(A, B, pi, a_alphabet, emitted)
    beta = estimate_beta(A, B, a_alphabet, emitted)

    gamma = estimate_gamma(alpha, beta, a_alphabet, emitted)
    eta = estimate_eta(alpha, beta, A, B, a_alphabet, emitted)

    A = estimate_a(gamma, eta, a_alphabet, emitted)
    B = estimate_b(gamma, a_alphabet, e_alphabet, emitted)
    #pi = estimate_pi(gamma)


print_matrix(A, a_alphabet, a_alphabet)
print('--------')
print_matrix(B, a_alphabet, e_alphabet)
