#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

const ll INF = 1e12;

vector<vector<pair<int, ll>>> g;
int n;

void solve() {
    cin >> n;
    g.resize(n + 1);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            ll x;
            cin >> x;
            if (x != 100000) {
                g[i].push_back({j, x});
            }
        }
        g[n].push_back({i, 0});
    }

    vector<int> p(n, -1);
    vector<ll> d(n + 1, INF);
    d[n] = 0;

    for (int k = 0; k <= n; ++k) {
        for (int i = 0; i <= n; ++i) {
            if (d[i] == INF) {
                continue;
            }
            for (auto [j, w]: g[i]) {
                if (d[j] > d[i] + w) {
                    d[j] = max((ll)(-1e9), d[i] + w);
                    p[j] = i;
                }
            }
        }
    }

    int v = -1;

    for (int i = 0; i < n; ++i) {
        for (auto [j, w]: g[i]) {
            if (d[j] > d[i] + w) {
                v = i;
                break;
            }
        }
    }

    if (v == -1) {
        cout << "NO" nl;
    } else {
        vector<int> cycle;
        for (int i = 0; i < n; ++i) {
             v = p[v]; // assume we are in a negative cycle
        }
        int i = v;
        while (p[i] != v) {
            cycle.push_back(i);
            i = p[i];
        }
        cycle.push_back(i);
        reverse(cycle.begin(), cycle.end());

        cout << "YES" nl << cycle.size() nl;
        for (auto &i: cycle) {
            cout << i+1 sq;
        }
        cout nl;
    }
}

/*



*/
