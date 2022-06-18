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

enum operation {
    ADD, REP, NONE
};

struct node {
    ll x = 0;
    ll mod = 0;
    operation op = NONE;
};

int n, m, logn;
vector<node> t;

/*

5 1000
1 0 3 3
2 2 4 2
3 1 3
2 1 5 1
1 0 2 2
3 0 3
3 3 5
1 0 5 0
2 0 5 1
3 1 4

*/

void push(int i, int tl, int tr) {
    switch (t[i].op) {
        case NONE:
            return;
        case ADD:
            t[i].x += t[i].mod * ll(tr - tl + 1);
            break;
        case REP:
            t[i].x = t[i].mod * ll(tr - tl + 1);
            break;
    }

    if (i >= logn) {
        t[i].mod = 0;
        t[i].op = NONE;
        return;
    }

    if (t[i].op == ADD) {
        if (t[2 * i].op == REP) {
            t[2 * i].mod += t[i].mod;
        } else {
            t[2 * i].op = ADD;
            t[2 * i].mod += t[i].mod;
        }
        if (t[2 * i + 1].op == REP) {
            t[2 * i + 1].mod += t[i].mod;
        } else {
            t[2 * i + 1].op = ADD;
            t[2 * i + 1].mod += t[i].mod;
        }

    } else if (t[i].op == REP) {
        t[2 * i].mod = t[i].mod;
        t[2 * i].op = REP;
        t[2 * i + 1].mod = t[i].mod;
        t[2 * i + 1].op = REP;
    }

    t[i].mod = 0;
    t[i].op = NONE;
}

void update(int i, int l, int r, int tl, int tr, ll delta, ll rep) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return;

    } else if (l <= tl && tr <= r) {
        if (rep == -1ll) {
            t[i].mod = delta;
            t[i].op = ADD;
        } else {
            t[i].mod = rep;
            t[i].op = REP;
        }
        push(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update(2 * i, l, r, tl, m, delta, rep);
        update(2 * i + 1, l, r, m + 1, tr, delta, rep);

        t[i].x = t[2 * i].x + t[2 * i + 1].x;
    }
}

ll query(int i, int l, int r, int tl, int tr) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return 0ll;

    } else if (l <= tl && tr <= r) {
        return t[i].x;

    } else {
            int m = (tl + tr) / 2;
            return query(2 * i, l, r, tl, m) + query(2 * i + 1, l, r, m + 1, tr);
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
            cin >> v;
            update(1, l, r - 1, 0, logn - 1, 0, v);

        } else if (type == 2) {
            cin >> v;
            update(1, l, r - 1, 0, logn - 1, v, -1);

        } else {
            cout << query(1, l, r - 1, 0, logn - 1) nl;
        }
    }
}

