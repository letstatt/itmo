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

int n, m, logn;
vector<ll> t;

inline void calc(int i) {
    t[i] = max(t[2 * i], t[2 * i + 1]);
}

void update(int i, ll x) {
    i += logn;
    t[i] = x;
    i /= 2;

    while (i > 0) {
        calc(i);
        i /= 2;
    }
}

int query(int i, int l, int r, int tl, int tr, ll x) {
    if (tr < l || tl > r || t[i] < x) {
        return INT_MAX;

    } else if (tl == tr) {
        return i;

    } else if (l <= tl && tr <= r) {
        int m = (tl + tr) / 2;
        return (t[2 * i] >= x ? query(2 * i, l, r, tl, m, x) :
                query(2 * i + 1, l, r, m + 1, tr, x));

    } else {
        int m = (tl + tr) / 2;
        return min(query(2 * i, l, r, tl, m, x), query(2 * i + 1, l, r, m + 1, tr, x));
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn, INT_MIN);

    for (int i = 0; i < n; ++i) {
        cin >> t[i + logn];
    }

    for (int i = logn - 1; i > 0; --i) {
        calc(i);
    }

    ll type, l, r;

    while (m--) {
        cin >> type >> l >> r;

        if (type == 1) {
            update(l, r);

        } else {
            int x = query(1, r, n - 1, 0, logn - 1, l);
            cout << (x == INT_MAX ? -1 : x - logn) nl;
        }
    }
}
