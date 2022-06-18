#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

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
int n, m, k, s;

void solve() {
    cin >> n >> m >> k >> s;
    g.resize(n);
    s--;

    for (int i = 0; i < m; ++i) {
        int x, y;
        ll w;
        cin >> x >> y >> w;
        x--, y--;
        g[x].push_back({y, w});
    }

    vector<vector<ll>> d(n, vector<ll>(k+1, INF));
    d[s][0] = 0;

    for (int len = 0; len < k; ++len) {
        for (int i = 0; i < n; ++i) {
            if (d[i][len] != INF) {
                for (auto [j, w]: g[i]) {
                    if (d[j][len + 1] > d[i][len] + w) {
                        d[j][len + 1] = max(-INF, d[i][len] + w);
                    }
                }
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        if (d[i][k] == INF) {
            cout << -1 nl;
        } else {
            cout << d[i][k] nl;
        }
    }
}
