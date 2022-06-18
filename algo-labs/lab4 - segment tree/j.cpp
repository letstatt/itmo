#pragma GCC optimization("Ofast")
#pragma GCC target("avx,avx2,fma")

#include <algorithm>
#include <iostream>
#include <climits>
#include <vector>
#include <cmath>
#include <set>

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
    int x = 0, mn = INT_MAX;
};

int n, m, logn;
vector<node> t;

void recalc(int i) {
    t[i].x = t[2 * i + 1].x + t[2 * i].x;
    t[i].mn = min(t[2 * i + 1].mn, t[2 * i].mn);
}

void update(int i, int j, int tl, int tr, int h) {
    if (tl == tr) {
        t[i].x = 1;
        t[i].mn = h;

    } else {
        int m = (tl + tr) / 2;
        if (j <= m) {
            update(2 * i, j, tl, m, h);
        } else {
            update(2 * i + 1, j, m + 1, tr, h);
        }
        recalc(i);
    }
}

int query(int i, int l, int r, int tl, int tr, int p) {
    if (tr < l || tl > r) {
        return 0;

    } else if (tl == tr) {
        if (t[i].mn <= p) {
            t[i].x = 0;
            t[i].mn = INT_MAX;
            return 1;
        }
        return 0;

    } else {
        if (t[i].mn > p) {
            return 0;
        }

        int m = (tl + tr) / 2;
        int sum = query(2 * i, l, r, tl, m, p) + query(2 * i + 1, l, r, m + 1, tr, p);

        if (sum > 0) {
            recalc(i);
        }

        return sum;
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn);

    int type, l, r;
    ll v;

    while (m--) {
        cin >> type >> l >> r;

        if (type == 1) {
            update(1, l, 0, logn - 1, r);

        } else {
            cin >> v;
            cout << query(1, l, r - 1, 0, logn - 1, v) nl;
        }
    }
}
