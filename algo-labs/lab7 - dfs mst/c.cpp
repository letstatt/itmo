#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

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

vector<vector<int>> g;
vector<int> used, tin, tup;
set<int> cutpoints;
int n, m, t = 0;

void dfs(int v, int p) {
    used[v] = 1;
    tin[v] = tup[v] = t++;
    int children = 0;

    for (auto &i: g[v]) {
        if (i == p) continue;

        if (used[i]) {
            tup[v] = min(tup[v], tin[i]);
        } else {
            dfs(i, v);
            tup[v] = min(tup[v], tup[i]);
            if (tup[i] >= tin[v] && p != -1) {
                cutpoints.insert(v);
            }
            children++;
        }
    }

    if (p == -1 && children > 1) {
        cutpoints.insert(v);
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
        g[x].push_back(y);
        g[y].push_back(x);
    }

    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            dfs(i, -1);
        }
    }

    cout << cutpoints.size() nl;
    for (auto &i: cutpoints) {
        cout << i+1 sq;
    }
    cout nl;
}
