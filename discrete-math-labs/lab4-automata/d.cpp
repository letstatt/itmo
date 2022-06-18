#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>

using namespace std;

typedef long long ll;

void solve();

int main() {
    freopen("problem4.in", "r", stdin);
    freopen("problem4.out", "w", stdout);
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, int> d;
    bool accepts = false;
    bool accessibility = false;
};

const ll mod = 1e9 + 7;
vector<vertex> g;
int n, m, k, l;

void topsort(int v, vector<int> &used, vector<int> &ts) {
    used[v] = 1;
    for (auto &i: g[v].d) {
        if (!used[i.second]) {
            topsort(i.second, used, ts);
        }
    }
    ts.push_back(v);
    used[v] = 2;
}

void solve() {
    cin >> n >> m >> k >> l;

    g.resize(n);

    for (int i = 0, j; i < k; ++i) {
        cin >> j;
        g[j - 1].accepts = true;
    }

    for (int i = 0, a, b; i < m; ++i) {
        char c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].d[c] = b;
    }

    vector<int> used(n);
    vector<int> ts;

    topsort(0, used, ts);

    //cout << "cock" << endl;

    reverse(ts.begin(), ts.end());
    vector<vector<ll>> d(l + 1, vector<ll>(n));
    d[0][0] = 1;

    for (int k = 0; k < l; ++k) {
        for (auto &i: ts) {
            for (auto &j: g[i].d) {
                d[k + 1][j.second] += d[k][i];
                d[k + 1][j.second] %= mod;
            }
        }
    }

    ll res = 0;

    for (int i = 0; i < n; ++i) {
        if (g[i].accepts) {
            res += d[l][i];
            res %= mod;
        }
    }

    cout << res << endl;
}
