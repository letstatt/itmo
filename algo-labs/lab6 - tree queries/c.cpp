#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <climits>
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

int n, m, logn;
vector<vector<int>> d, p;
vector<int> h;

int min_edge_calc(int x, int y) {
    int min_edge = INT_MAX;

    if (h[x] > h[y]) {
        swap(x, y);
    }

    // h[x] <= h[y] there

    for (int j = logn - 1; j >= 0; --j) {
        if (h[y] - (1 << j) >= h[x]) {
            min_edge = min(min_edge, d[y][j]);
            y = p[y][j];
        }
    }

    if (x == y) {
        return min_edge;
    }

    for (int j = logn - 1; j >= 0; --j) {
        if (p[x][j] != p[y][j]) {
            min_edge = min(min_edge, d[x][j]);
            min_edge = min(min_edge, d[y][j]);
            x = p[x][j];
            y = p[y][j];
        }
    }
    return min(min_edge, min(d[x][0], d[y][0]));
}

void solve() {
    cin >> n;
    logn = floor(log2(n) + 1);
    p.resize(n + 1, vector<int>(logn));
    d.resize(n + 1, vector<int>(logn, INT_MAX));
    h.resize(n + 1);

    for (int i = 2; i <= n; ++i) {
        cin >> p[i][0] >> d[i][0];
        h[i] = h[p[i][0]] + 1;
    }

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            p[i][j] = p[p[i][j - 1]][j - 1];
            d[i][j] = min(d[i][j - 1], d[p[i][j - 1]][j - 1]);
        }
    }

    cin >> m;

    while (m--) {
        int x, y;
        cin >> x >> y;
        cout << min_edge_calc(x, y) << endl;
    }
}