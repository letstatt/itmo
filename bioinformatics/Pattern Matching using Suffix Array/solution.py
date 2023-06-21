
def suffix_array(s):
    # not optimal one solution, but quick to code (n^2*logn)
    return sorted(range(len(s)), key=lambda i: s[i:])


def match(p, s, i):
    for j in range(len(p)):
        if i + j >= len(s):
            return 1
        if p[j] < s[i + j]:
            return -1
        elif p[j] == s[i + j]:
            continue
        else:
            return 1
    return 0


def binary_search(l, r, comparer, lower_bound=True):
    while r - l > 1:
        m = (l + r) // 2
        cmp = comparer(m)
        if (lower_bound and cmp <= 0) or (not lower_bound and cmp < 0):
            r = m
        else:
            l = m

    l_cmp = comparer(l)
    r_cmp = comparer(r)

    if l_cmp and r_cmp:
        return None
    elif lower_bound:
        return l if not l_cmp else r
    else:
        return r if not r_cmp else l


def pattern_matching(sa, s, p):
    # (m*logn)
    if len(p) > len(s):
        return []
    
    lower_bound = binary_search(0, len(sa) - 1, lambda m: match(p, s, sa[m]))
    if lower_bound is None:
        return []
    
    upper_bound = binary_search(0, len(sa) - 1, lambda m: match(p, s, sa[m]), lower_bound=False)
    return (sa[i] for i in range(lower_bound, upper_bound + 1))
    

if __name__ == "__main__":
    s = input()
    sa = suffix_array(s)
    occurences = set()

    while True:
        try:
            p = input()
            occurences = occurences.union(set(pattern_matching(sa, s, p)))
        except EOFError:
            break

    print(' '.join(map(str, sorted(occurences))))
