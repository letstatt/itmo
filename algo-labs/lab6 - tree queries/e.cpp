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
#endif
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

const int COLORED = 1;

int n, m, logn;
vector<vector<int>> g, p, c;
vector<int> h;

void dfs(int v, int parent = 0) {
    h[v] = h[parent] + 1;

    for (auto &i: g[v]) {
        if (!h[i]) {
            p[i][0] = v;
            dfs(i, v);
        }
    }
}

int lca(int x, int y) {
    if (h[x] > h[y]) {
        swap(x, y);
    }

    // h[x] <= h[y] there

    for (int j = logn - 1; j >= 0; --j) {
        if (h[y] - (1 << j) >= h[x]) {
            c[y][j] = COLORED;
            y = p[y][j];
        }
    }

    if (x == y) {
        return x;
    }

    for (int j = logn - 1; j >= 0; --j) {
        if (p[x][j] != p[y][j]) {
            c[x][j] = COLORED;
            c[y][j] = COLORED;
            x = p[x][j];
            y = p[y][j];
        }
    }
    c[x][0] = COLORED;
    c[y][0] = COLORED;
    return p[x][0];
}

void solve() {
    cin >> n;
    logn = floor(log2(n) + 1);
    p.resize(n + 1, vector<int>(logn));
    c.resize(n + 1, vector<int>(logn));
    h.resize(n + 1);
    g.resize(n + 1);

    for (int i = 1, x, y; i < n; ++i) {
        cin >> x >> y;
        g[x].push_back(y);
        g[y].push_back(x);
    }

    dfs(1); // root = 1
    fill(p[1].begin(), p[1].end(), 1);

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            p[i][j] = p[p[i][j - 1]][j - 1];
        }
    }

    cin >> m;

    int x, y;

    while (m--) {
        cin >> x >> y;
        lca(x, y);
    }

    for (int j = logn - 1; j >= 1; --j) {
        for (int i = 1; i <= n; ++i) {
            if (c[i][j] == COLORED) {
                c[i][j - 1] = COLORED;
                c[p[i][j - 1]][j - 1] = COLORED;
            }
        }
    }

    int ans = n - 1;

    for (int i = 2; i <= n; ++i) {
        ans -= (c[i][0] == COLORED);
    }

    cout << ans nl;
}