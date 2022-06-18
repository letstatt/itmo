#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <climits>
#include <random>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
#ifndef LOCAL
    //freopen("input.txt", "r", stdin);
    //freopen("output.txt", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
#endif
    solve();
}

struct node {
    ll x = 0;
    ll push = 0;
};

int n, m, tree_n, logn, timer = 0;
vector<vector<int>> g, p;
vector<int> h, tin, tout;
vector<node> t;

void dfs(int v, int parent = 0) {
    h[v] = h[parent] + 1;
    tin[v] = timer++;
    for (auto &i: g[v]) {
        if (i != parent) {
            p[i][0] = v;
            dfs(i, v);
        }
    }
    tout[v] = timer;
}

void push (int v) {
    if (v < tree_n) {
        t[2 * v].push += t[v].push;
        t[2 * v + 1].push += t[v].push;
    }
    t[v].x += t[v].push;
    t[v].push = 0;
}

void update(int v, int l, int r, int tl, int tr, ll delta) {
    push(v);
    if (tl > r || tr < l) {
        return;
    } else if (l <= tl && tr <= r) {
        t[v].push += delta;
        push(v);
        return;
    }
    int tm = (tl + tr) / 2;
    update(2 * v, l, r, tl, tm, delta);
    update(2 * v + 1, l, r, tm + 1, tr, delta);
    t[v].x = t[2 * v].x + t[2 * v + 1].x;
}

ll get(int v, int l, int r, int tl, int tr) {
    push(v);
    if (tl > r || tr < l) {
        return 0ll;
    } else if (l <= tl && tr <= r) {
        return t[v].x;
    }
    int tm = (tl + tr) / 2;
    return get(2 * v, l, r, tl, tm) + get(2 * v + 1, l, r, tm + 1, tr);
}

int lca(int x, int y) {
    if (h[x] > h[y]) {
        swap(x, y);
    }
    // h[x] <= h[y] there
    for (int j = logn - 1; j >= 0; --j) {
        if (h[y] - (1 << j) >= h[x]) {
            y = p[y][j];
        }
    }

    if (x == y) {
        return x;
    }

    for (int j = logn - 1; j >= 0; --j) {
        if (p[x][j] != p[y][j]) {
            x = p[x][j];
            y = p[y][j];
        }
    }
    return p[x][0];
}

void add(int x, int y, int z, long long val) {
    update(1, tin[x], tin[x], 0, tree_n - 1, val);
    update(1, tin[y], tin[y], 0, tree_n - 1, val);
    update(1, tin[z], tin[z], 0, tree_n - 1, -val);
    if (p[z][0]) {
        update(1, tin[p[z][0]], tin[p[z][0]], 0, tree_n - 1, -val);
    }
}

void solve() {
    cin >> n;
    logn = floor(log2(n + 1) + 1);
    tree_n = 1 << int(log2(n) + 1);
    p.resize(n + 1, vector<int>(logn));
    t.resize(tree_n * 2);
    g.resize(n + 1);
    h.resize(n + 1);
    tin.resize(n + 1);
    tout.resize(n + 1);

    for (int i = 1, x, y; i < n; ++i) {
        cin >> x >> y;
        g[x].push_back(y);
        g[y].push_back(x);
    }

    dfs(1);

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            p[i][j] = p[p[i][j - 1]][j - 1];
        }
    }

    char c;
    int x, y;
    long long val;
    cin >> m;

    while (m--) {
        cin >> c;

        if (c == '+') {
            cin >> x >> y >> val;
            add(x, y, lca(x, y), val);
        } else if (c == '?') {
            cin >> x;
            cout << get(1, tin[x], tout[x] - 1, 0, tree_n - 1) nl;
        }
    }
}