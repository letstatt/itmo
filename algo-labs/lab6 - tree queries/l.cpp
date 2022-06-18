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

int n, m, logn;
vector<vector<int>> g, pp;
vector<int> h, p;

// p - centroid decomposition tree
// h - centroid height
// pp - lca

vector<int> d, c, order;
vector<vector<pair<ll, int>>> sum_b, sum_w;

// d - depth
// c - color
// order - vertex order in children array
// sum_b - Fenwik tree of distances to black vertexes and count of black vertexes
// sum_w - Fenwik tree of distances to white vertexes and count of white vertexes

int dfs(int v, int size, int &center, int parent = -1) {
    int sum = 1;
    for (auto x : g[v]) {
        if (h[x] == -1 && x != parent) {
            sum += dfs(x, size, center, v);
        }
    }
    if (center == -1 && (2 * sum >= size || parent == -1)) {
        center = v;
    }
    return sum;
}

void build(int v, int size, int depth, int centroid_parent) {
    int center = -1;
    dfs(v, size, center);

    p[center] = centroid_parent;
    h[center] = depth;

    if (centroid_parent != -1) {
        order[center] = (int)sum_b[centroid_parent].size();
        sum_b[centroid_parent].push_back(pair<int, int>());
        sum_w[centroid_parent].push_back(pair<int, int>());
    }

    for (auto x: g[center]) {
        if (h[x] == -1) {
            build(x, size / 2, depth + 1, center);
        }
    }
}

void fenwick_update(vector<pair<ll, int>> &t, int i, ll delta) {
    /*while (i < t.size()) {
        t[i] += delta;
        i = i | (i + 1);
    }*/
    t[i].first += delta;
}
void fenwick_update2(vector<pair<ll, int>> &t, int i, int delta) {
    /*while (i < t.size()) {
        t[i] += delta;
        i = i | (i + 1);
    }*/
    t[i].second += delta;
}

void fenwick_set(vector<pair<ll, int>> &t, int i, ll x) {
    /*int delta = x - t[i];
    t[i] = x;
    fenwick_update(t, i, delta);*/
    t[i].first = x;
}

ll fenwick_sum(vector<pair<ll, int>> &t, int i) {
    ll res = 0;
    while (i >= 0) {
        res += t[i].first;
        i = (i & (i + 1)) - 1;
    }
    return res;
}

ll fenwick_get(vector<pair<ll, int>> &t, int i) {
    //return fenwick_sum(t, i + 1) - fenwick_sum(t, i);
    return t[i].first;
}

ll fenwick_get(vector<pair<ll, int>> &t, int l, int r) {
    ll res = 0;
    for (int i = l; i < r; ++i) {
        res += t[i].first;
    }
    return res;
}

ll fenwick_get2(vector<pair<ll, int>> &t, int l, int r) {
    ll res = 0;
    for (int i = l; i < r; ++i) {
        res += t[i].second;
    }
    return res;
}

int lca(int x, int y) {
    if (d[x] > d[y]) {
        swap(x, y);
    }

    // d[x] <= d[y] there

    for (int j = logn - 1; j >= 0; --j) {
        if (d[y] - (1 << j) >= d[x]) {
            y = pp[y][j];
        }
    }

    if (x == y) {
        return x;
    }

    for (int j = logn - 1; j >= 0; --j) {
        if (pp[x][j] != pp[y][j]) {
            x = pp[x][j];
            y = pp[y][j];
        }
    }
    return pp[x][0];
}

ll length(int u, int v) {
    int z = lca(u, v);
    return abs(d[u] - d[z]) + abs(d[v] - d[z]);
}

void update(int v, vector<vector<pair<ll, int>>> &sums, int multiplier) {
    int centroid = p[v];
    int child = v;
    while (centroid != -1) {
        fenwick_update(sums[centroid], order[child], multiplier * length(centroid, v));
        sums[centroid][order[child]].second += multiplier;
        child = centroid;
        centroid = p[centroid];
    }
}

void init(int v, int parent = 0) {
    d[v] = d[parent] + 1;
    for (int i = 0; i < g[v].size(); ++i) {
        int u = g[v][i];
        if (u != parent) {
            init(u, v);
        }
    }
    pp[v][0] = parent;
}

ll query(int v) {
    auto & sums = (c[v] == 0 ? sum_b : sum_w);
    ll res = fenwick_get(sums[v], 0, (int)sums[v].size());
    int centroid = p[v];
    int child = v;

    while (centroid != -1) {
        ll tmp = fenwick_get(sums[centroid], order[child]);
        int tmp2 = sums[centroid][order[child]].second;
        fenwick_update(sums[centroid], order[child], -tmp);
        fenwick_update2(sums[centroid], order[child], -tmp2);
        res += fenwick_get(sums[centroid], 0, (int)sums[centroid].size());
        res += fenwick_get2(sums[centroid], 0, (int)sums[centroid].size()) * length(centroid, v);
        if (c[v] == c[centroid]) {
            res += length(centroid, v);
        }
        fenwick_update(sums[centroid], order[child], +tmp);
        fenwick_update2(sums[centroid], order[child], +tmp2);
        child = centroid;
        centroid = p[centroid];
    }
    return res;
}

void solve() {
    cin >> n >> m;
    h.resize(n + 1, -1);
    p.resize(n + 1);
    g.resize(n + 1);

    logn = floor(log2(n) + 1);
    pp.resize(n + 1, vector<int>(logn));

    d.resize(n + 1);
    c.resize(n + 1);
    order.resize(n + 1);
    sum_w.resize(n + 1);
    sum_b.resize(n + 1);

    for (int i = 1, x, y; i < n; ++i) {
        cin >> x >> y;
        g[x].push_back(y);
        g[y].push_back(x);
    }

    build(1, n, 0, -1);
    init(1);

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            pp[i][j] = pp[pp[i][j - 1]][j - 1];
        }
    }

    for (int i = 1; i <= n; ++i) {
        update(i, sum_b, 1);
    }

    int t, v;

    while (m--) {
        cin >> t >> v;

        if (t == 1) {
            if (c[v] == 0) { // black
                update(v, sum_b, -1);
                update(v, sum_w, 1);
            } else { // white
                update(v, sum_w, -1);
                update(v, sum_b, 1);
            }
            c[v] ^= 1;
        } else {
            cout << query(v) << endl;
        }
    }
}