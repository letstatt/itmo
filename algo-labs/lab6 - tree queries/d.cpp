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

int n, m, logn, t = 2;
vector<vector<int>> p;
vector<int> h, dsu;

int getp(int x) {
    return dsu[x] == x ? x : dsu[x] = getp(dsu[x]);
}

void unite(int x, int y) {
    int a = getp(x);
    int b = getp(y);

    if (a == b) return;
    dsu[a] = b;
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

void solve() {
    cin >> m;
    n = m;
    logn = floor(log2(n) + 1);
    p.resize(n + 1, vector<int>(logn));
    dsu.resize(n + 1);
    h.resize(n + 1);

    dsu[1] = 1;

    char c;
    int x, y;

    while (m--) {
        cin >> c;

        if (c == '+') {
            cin >> x;
            dsu[t] = t;
            p[t][0] = x;
            h[t] = h[x] + 1;

            for (int j = 1; j < logn; ++j) {
                p[t][j] = p[p[t][j - 1]][j - 1];
            }
            ++t;
        } else if (c == '-') {
            cin >> x;
            unite(x, p[x][0]);
        } else if (c == '?') {
            cin >> x >> y;
            cout << getp(lca(x, y)) nl;
        }
    }
}