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

struct node {
    ll x = 0, add = -1;
};

int n, m, logn;
vector<node> t;

void push(int i, int tl, int tr) {
    if (t[i].add < 0) {
        return;

    } else if (i >= logn) {
        t[i].x = t[i].add;
        t[i].add = -1;
        return;
    }

    t[i].x = t[i].add;
    t[2 * i].add = t[i].add;
    t[2 * i + 1].add = t[i].add;
    t[i].add = -1;
}

void update(int i, int l, int r, int tl, int tr, ll delta) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return;

    } else if (l <= tl && tr <= r) {
        t[i].add = delta;
        push(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update(2 * i, l, r, tl, m, delta);
        update(2 * i + 1, l, r, m + 1, tr, delta);
        t[i].x = min(t[2 * i].x, t[2 * i + 1].x);
    }
}

ll query(int i, int l, int r, int tl, int tr) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return LLONG_MAX;

    } else if (l <= tl && tr <= r) {
        return t[i].x;

    } else {
            int m = (tl + tr) / 2;
            return min(query(2 * i, l, r, tl, m), query(2 * i + 1, l, r, m + 1, tr));
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn, {LLONG_MAX, -1});

    for (int i = 0; i < n; ++i) {
        t[i + logn].x = 0;
    }

    for (int i = logn - 1; i > 0; --i) {
        t[i].x = min(t[2 * i].x, t[2 * i + 1].x);
    }

    int type, l, r;
    ll v;

    while (m--) {
        cin >> type >> l >> r;

        if (type == 1) {
            cin >> v;
            update(1, l, r - 1, 0, logn - 1, v);

        } else {
            cout << query(1, l, r - 1, 0, logn - 1) nl;
        }
    }
}
