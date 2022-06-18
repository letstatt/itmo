#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<vector<pair<int, int>>> g;
vector<int> used, tin, tup, bridges;
int n, m, t = 0;

void dfs(int v, int p) {
    used[v] = 1;
    tin[v] = tup[v] = t++;

    for (auto &e: g[v]) {
        auto [i, j] = e;
        if (i == p) continue;

        if (used[i]) {
            tup[v] = min(tup[v], tin[i]);
        } else {
            dfs(i, v);
            tup[v] = min(tup[v], tup[i]);
            if (tup[i] > tin[v]) {
                bridges.push_back(j);
            }
        }
    }
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    used.resize(n);
    tin.resize(n);
    tup.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back({y, i});
        g[y].push_back({x, i});
    }

    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            dfs(i, -1);
        }
    }

    sort(bridges.begin(), bridges.end());

    cout << bridges.size() nl;
    for (auto &i: bridges) {
        cout << i+1 sq;
    }
    cout nl;
}
