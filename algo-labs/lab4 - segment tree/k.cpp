#include <algorithm>
#include <iostream>
#include <climits>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    solve();
}

enum color {
    WHITE, BLACK, NONE
};

struct node {
    int cnt = 0, sum = 0;
    color l = WHITE, r = WHITE;
    color op = NONE;
};

int m, logn;
vector<node> t;

inline node unite(const node& a, const node& b) {
    return {max(0, a.cnt + b.cnt - (a.r == b.l && a.r == BLACK)), a.sum + b.sum, a.l, b.r, NONE};
}

void push(int i, int tl, int tr) {
    if (t[i].op == NONE) {
        return;
    }

    t[i].l = t[i].r = t[i].op;
    t[i].cnt = (t[i].op == BLACK);
    t[i].sum = (tr - tl + 1) * (t[i].op == BLACK);

    if (i >= logn) {
        t[i].op = NONE;
        return;
    }

    t[2 * i].op = t[2 * i + 1].op = t[i].op;
    t[i].op = NONE;
}

void update(int i, int l, int r, int tl, int tr, color col) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return;

    } else if (l <= tl && tr <= r) {
        t[i].op = col;
        push(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update(2 * i, l, r, tl, m, col);
        update(2 * i + 1, l, r, m + 1, tr, col);

        t[i] = unite(t[2 * i], t[2 * i + 1]);
    }
}

void solve() {
    cin >> m;
    //logn = 1 << int(log2(n - 1) + 1);
    logn = 1048576;

    t.resize(2 * logn);

    char color;
    int l, r, length;

    while (m--) {
        cin >> color >> l >> length;
        l += 500000;
        r = l + length - 1;

        update(1, l, r, 0, logn - 1, color == 'B' ? BLACK : WHITE);
        cout << t[1].cnt sp t[1].sum nl;
    }
}

