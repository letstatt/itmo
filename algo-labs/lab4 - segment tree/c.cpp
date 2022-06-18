#include <iostream>
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
    ll pref, suff, sum, ans;

    node() {
        sum = -1e9;
        pref = suff = ans = 0;
    }

    node(ll x) {
        sum = x;
        pref = suff = ans = max(0ll, x);
    }
};

int n, m, logn;
vector<node> t;

void calc(int i) {
    t[i].pref = max(t[2 * i].pref, t[2 * i].sum + t[2 * i + 1].pref);
    t[i].suff = max(t[2 * i + 1].suff, t[2 * i + 1].sum + t[2 * i].suff);
    t[i].sum = t[2 * i].sum + t[2 * i + 1].sum;
    t[i].ans = max(max(t[2 * i].ans, t[2 * i + 1].ans), t[2 * i].suff + t[2 * i + 1].pref);
}

void update(int i, ll x) {
    i += logn;
    t[i] = node(x);
    i /= 2;

    while (i > 0) {
        calc(i);
        i /= 2;
    }
}

void build(int i) {
    if (i < logn) {
        build(2 * i);
        build(2 * i + 1);
        calc(i);
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn);

    for (int i = 0, x; i < n; ++i) {
        cin >> x;
        t[logn + i] = node((ll) x);
    }

    build(1);

    int i, v;
    cout << t[1].ans nl;

    while (m--) {
        cin >> i >> v;
        update(i, (ll) v);
        cout << t[1].ans nl;
    }
}
