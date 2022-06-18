#include <algorithm>
#include <iostream>
#include <vector>
#include <queue>

using namespace std;

#define nl << '\n'

void solve();

int n;

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.cie(0);*/
    while (cin >> n) {
        solve();
    }
}


void solve() {
    vector<vector<int>> g, gr;
    vector<int> d, deg;
    int m;

    cin >> m;

    g.resize(n);
    gr.resize(n);
    deg.resize(n);
    d.resize(n, -1);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back(y);
        gr[y].push_back(x);
        deg[x]++;
    }

    queue<int> q;

    for (int i = 0; i < n; ++i) {
        if (g[i].size() == 0) {
            q.push(i);
            d[i] = 0;
        }
    }

    while (!q.empty()) {
        int v = q.front();
        q.pop();

        if (d[v] == 0) {
            for (auto u: gr[v]) {
                if (d[u] == -1) {
                    d[u] = 1;
                    q.push(u);
                }
            }
        } else {
            for (auto u: gr[v]) {
                deg[u]--;
                if (!deg[u]) {
                    d[u] = 0;
                    q.push(u);
                }
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        if (d[i] == 0) {
            cout << "SECOND" nl;
        } else if (d[i] == 1) {
            cout << "FIRST" nl;
        } else {
            cout << "DRAW" nl;
        }
    }
    cout nl;
}
