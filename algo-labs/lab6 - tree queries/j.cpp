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

int n;
vector<vector<int>> g;
vector<int> h, p;

// p - centroid decomposition tree
// h - centroid height

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

    for (auto x: g[center]) {
        if (h[x] == -1) {
            build(x, size / 2, depth + 1, center);
        }
    }
};


void solve() {
    cin >> n;
    h.resize(n + 1, -1);
    p.resize(n + 1);
    g.resize(n + 1);

    for (int i = 1, x, y; i < n; ++i) {
        cin >> x >> y;
        g[x].push_back(y);
        g[y].push_back(x);
    }

    build(1, n, 0, -1);

    for (int i = 1; i <= n; ++i) {
        cout << (p[i] == -1 ? 0 : p[i]) << " ";
    }
}