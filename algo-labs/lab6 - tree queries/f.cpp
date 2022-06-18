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

int n, m, logn, root;
vector<vector<int>> g;
vector<int> p, h, w, heavy, remap, comp, hld_p, used, hld_d;

void dfs(int v, int parent = 0) {
    h[v] = h[parent] + 1;

    for (auto &i: g[v]) {
        dfs(i, v);
        w[v] += w[i];
        if (w[heavy[v]] < w[i]) {
            heavy[v] = i;
        }
    }
}

void build_hld(int v, int &t_remap, int &t_comp) {
    if (!hld_p[t_comp]) {
        hld_p[t_comp] = t_remap;
    }
    comp[v] = t_comp;
    remap[t_remap++] = v;
    if (heavy[v]) {
        build_hld(heavy[v], t_remap, t_comp);
    }
    for (auto &i: g[v]) {
        if (heavy[v] != i) {
            ++t_comp;
            build_hld(i, t_remap, t_comp);
        }
    }
}

void precalc() {
    w.resize(n + 1, 1);
    heavy.resize(n + 1);
    h.resize(n + 1);

    dfs(root);

    int t_remap = 1;
    int t_comp = 1;
    comp.resize(n + 1);
    remap.resize(n + 1);
    hld_p.resize(n + 1);

    build_hld(root, t_remap, t_comp);

    // preparing for giving answers
    used.resize(n + 1);
    hld_d.resize(n + 1);
}

void query_hld(int v, int &t_query, int &res) {
    int r = remap[hld_p[comp[v]]];
    if (used[r] < t_query) {
        used[r] = t_query;
        hld_d[r] = h[r];
    }

    res += max(0, h[v] + 1 - hld_d[r]);
    hld_d[r] = max(h[v] + 1, hld_d[r]);

    if (p[r]) {
        query_hld(p[r], t_query, res);
    }
}

void solve() {
    cin >> n;
    logn = floor(log2(n) + 1);
    p.resize(n + 1);
    g.resize(n + 1);

    for (int i = 1, j; i <= n; ++i) {
        cin >> j;
        if (j != -1) {
            g[j].push_back(i);
            p[i] = j;
        } else {
            root = i;
        }
    }

    precalc();
    cin >> m;

    for (int i = 1, k, x, ans = 0; i <= m; ans = 0, ++i) {
        cin >> k;
        while (k--) {
            cin >> x;
            query_hld(x, i, ans);
        }
        cout << ans nl;
    }
}