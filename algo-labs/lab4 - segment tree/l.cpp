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
    ll x = 0, y = 0;
};

int n, m, logn;
vector<node> t1, t2;

void push1(int i, int tl, int tr) {
    if (t1[i].y == 0) {
        return;
    }

    t1[i].x += (tr - tl + 1) * t1[i].y;

    if (i < logn) {
        t1[2 * i].y += t1[i].y;
        t1[2 * i + 1].y += t1[i].y;
    }

    t1[i].y = 0;
}

void update1(int i, int l, int r, int tl, int tr, ll x) {
    push1(i, tl, tr);

    if (tl > r || tr < l) {
        return;

    } else if (l <= tl && tr <= r) {
        t1[i].y = x;
        push1(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update1(2 * i, l, r, tl, m, x);
        update1(2 * i + 1, l, r, m + 1, tr, x);
        t1[i].x = t1[2 * i].x + t1[2 * i + 1].x;
    }
}

ll query1(int i, int l, int r, int tl, int tr) {
    push1(i, tl, tr);

    if (tl > r || tr < l) {
        return 0;

    } else if (l <= tl && tr <= r) {
        return t1[i].x;

    } else {
        int m = (tl + tr) / 2;
        return query1(2 * i, l, r, tl, m) + query1(2 * i + 1, l, r, m + 1, tr);
    }
}

void push2(int i, int tl, int tr) {
    if (t2[i].y == 0) {
        return;
    }

    t2[i].x += (((tl + 1) * t2[i].y + (tr + 1) * t2[i].y) * (tr - tl + 1)) / 2;

    if (i < logn) {
        t2[2 * i].y += t2[i].y;
        t2[2 * i + 1].y += t2[i].y;
    }

    t2[i].y = 0;
}

void update2(int i, int l, int r, int tl, int tr, ll x) {
    push2(i, tl, tr);

    if (tl > r || tr < l) {
        return;

    } else if (l <= tl && tr <= r) {
        t2[i].y = x;
        push2(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update2(2 * i, l, r, tl, m, x);
        update2(2 * i + 1, l, r, m + 1, tr, x);
        t2[i].x = t2[2 * i].x + t2[2 * i + 1].x;
    }
}

ll query2(int i, int l, int r, int tl, int tr) {
    push2(i, tl, tr);

    if (tl > r || tr < l) {
        return 0;

    } else if (l <= tl && tr <= r) {
        return t2[i].x;

    } else {
        int m = (tl + tr) / 2;
        return query2(2 * i, l, r, tl, m) + query2(2 * i + 1, l, r, m + 1, tr);
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t1.resize(2 * logn);
    t2.resize(2 * logn);

    for (int i = 0; i < n; ++i) {
        cin >> t1[i + logn].x;
        t2[i + logn].x = (i + 1) * t1[i + logn].x;
    }

    for (int i = logn - 1; i > 0; --i) {
        t1[i].x = t1[2 * i].x + t1[2 * i + 1].x;
        t2[i].x = t2[2 * i].x + t2[2 * i + 1].x;
    }

    int type, l, r;
    ll x;

    while (m--) {
        /*for (int i = 0; i < n; ++i) {
            cout << t1[i + logn].y sq;
        }
        cout nl;
        for (int i = 0; i < n; ++i) {
            cout << query1(1, i, i, 0, logn - 1) sq;
        }
        cout nl;
        for (int i = 0; i < n; ++i) {
            cout << t1[i + logn].y sq;
        }
        cout nl;
        for (int i = 0; i < n; ++i) {
            cout << query2(1, i, i, 0, logn - 1) sq;
        }
        cout nl;*/

        cin >> type >> l >> r;

        if (type == 1) {
            cin >> x;
            update1(1, l - 1, r - 1, 0, logn - 1, x);
            update2(1, l - 1, r - 1, 0, logn - 1, x);

        } else {
            ll res = query2(1, l - 1, r - 1, 0, logn - 1);
            ll res2 = res - (l - 1) * query1(1, l - 1, r - 1, 0, logn - 1);
            cout << res2 nl;
        }
    }
}
