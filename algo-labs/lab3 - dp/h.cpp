#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n;
vector<vector<ll>> g, d, p;

void solve() {
    cin >> n;
    g.resize(n, vector<ll>(n));
    d.resize((1 << n) + 1, vector<ll>(n, 1e10));
    p.resize((1 << n) + 1, vector<ll>(n, -1));

    for (auto &i: g) {
        for (auto &j: i) cin >> j;
    }

    vector<pair<int, int>> q;
    int it = 0;

    for (int i = 0; i < n; ++i) {
        q.push_back({1 << i, i});
        d[1 << i][i] = 0;
    }

    while (it < q.size()) {
        int m = q[it].first;
        int u = q[it++].second;

        for (int i = 0; i < n; ++i) {
            if ((m & (1 << i)) == 0 && d[m][u] + g[u][i] < d[m | (1 << i)][i]) {
                d[m | (1 << i)][i] = d[m][u] + g[u][i];
                q.push_back({m | (1 << i), i});
                p[m | (1 << i)][i] = u;
            }
        }
    }

    ll best = 1e10;
    int m = (1 << n) - 1;
    int v = -1;

    for (int i = 0; i < n; ++i) {
        if (d[m][i] < best) {
            best = d[m][i];
            v = i;
        }
    }

    vector<int> path;

    while (v != -1) {
        path.push_back(v + 1);
        v = p[m][v];
        m ^= 1 << (path.back() - 1);
    }

    reverse(path.begin(), path.end());

    cout << best nl;
    for (auto &i: path) cout << i sq;
    cout nl;

}
