#pragma GCC optimization("Ofast")
#pragma GCC target("avx,avx2,fma")

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

ll R;

struct node {
    int a[2][2];

    node() {
        a[0][0] = a[1][1] = 1;
        a[0][1] = a[1][0] = 0;
    }

    node operator* (const node& b) const {
        node x = node();
        x.a[0][0] = x.a[1][1] = 0;

        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                for (int k = 0; k < 2; ++k) {
                    x.a[i][j] += a[i][k] * b.a[k][j];
                }
                x.a[i][j] %= R;
            }
        }

        return x;
    }

    bool operator!= (const node& b) const {
        return a != b.a;
    }
};

int n, m, logn;
vector<node> t;

node identity_matrix;

node query(int i, int l, int r, int tl, int tr) {
    if (tr < l || tl > r) {
        return identity_matrix;

    } else if (l <= tl && tr <= r) {
        return t[i];

    } else {
        int m = (tl + tr) / 2;

        node a = query(2 * i, l, r, tl, m);
        node b = query(2 * i + 1, l, r, m + 1, tr);

        if (a != identity_matrix && b != identity_matrix) {
            return a * b;
        }

        return (a != identity_matrix ? a : b);
    }
}

void solve() {
    cin >> R >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    identity_matrix = node();
    t.resize(2 * logn);

    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                cin >> t[k + logn].a[i][j];
                t[k + logn].a[i][j] %= R;
            }
        }
    }

    for (int i = logn - 1; i > 0; --i) {
        t[i] = t[2 * i] * t[2 * i + 1];
    }

    int l, r;

    while (m--) {
        cin >> l >> r;
        node x = query(1, l - 1, r - 1, 0, logn - 1);

        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                cout << x.a[i][j] sq;
            }
            cout nl;
        }
        cout nl;
    }
}

