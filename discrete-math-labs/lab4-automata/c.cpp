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
    freopen("problem3.in", "r", stdin);
    freopen("problem3.out", "w", stdout);
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
vector<vertex> g, gr;
int n, m, k;

void dfsr(int v) {
    g[v].accessibility = true;
    for (auto &i: gr[v].d) {
        if (!g[i.second].accessibility) {
            dfsr(i.second);
        }
    }
}

bool topsort(int v, vector<int> &used, vector<int> &ts) {
    used[v] = 1;
    for (auto &i: g[v].d) {
        if (!used[i.second] && !topsort(i.second, used, ts)) {
            return false;

        } else if (used[i.second] == 1 && g[i.second].accessibility) {
            return false;
        }
    }
    ts.push_back(v);
    used[v] = 2;
    return true;
}

void solve() {
    cin >> n >> m >> k;

    g.resize(n);
    gr.resize(n);

    for (int i = 0, j; i < k; ++i) {
        cin >> j;
        g[j - 1].accepts = true;
    }

    for (int i = 0, a, b; i < m; ++i) {
        char c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].d[c] = b;
        gr[b].d[c] = a;
    }

    for (int i = 0; i < n; ++i) {
        if (g[i].accepts && !g[i].accessibility) {
            dfsr(i);
        }
    }

    vector<int> used(n);
    vector<int> ts;

    if (!topsort(0, used, ts)) {
        cout << "-1" << endl;
        return;
    }

    reverse(ts.begin(), ts.end());
    vector<ll> d(n);
    d[0] = 1;

    for (auto &i: ts) {
        for (auto &j: g[i].d) {
            d[j.second] += d[i];
            d[j.second] %= mod;
        }
    }

    ll res = 0;

    for (int i = 0; i < n; ++i) {
        if (g[i].accepts) {
            res += d[i];
            res %= mod;
        }
    }

    cout << res << endl;
}
