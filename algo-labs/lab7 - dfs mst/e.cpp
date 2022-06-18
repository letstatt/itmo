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

vector<vector<pair<int, int>>> g;
vector<int> used, tin, tup, buf;
vector<int> comp;
int n, m, t = 0, c = 0;

/*
5 7
1 2
2 3
3 1
1 4
4 5
5 1
3 4
*/

void dfs(int v, int p) {
    used[v] = 1;
    tin[v] = tup[v] = t++;

    int to_parent = 0;

    for (auto &e: g[v]) {
        auto [i, j] = e;
        if (i == p) {
            to_parent++;
            if (to_parent > 1) {
                tup[v] = min(tup[v], tin[i]);
                buf.push_back(j);
            }
            continue;
        }

        if (used[i]) {
            if (tin[i] < tin[v]) {
                buf.push_back(j);
            }
            tup[v] = min(tup[v], tin[i]);
        } else {
            buf.push_back(j);
            dfs(i, v);
            tup[v] = min(tup[v], tup[i]);

            if (tup[i] >= tin[v]) {
                while (true) {
                    int x = buf.back();
                    comp[x] = c;
                    buf.pop_back();
                    if (x == j) {
                        break;
                    }
                }
                c++;
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
    comp.resize(m);

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

    cout << c nl;
    for (auto &i: comp) {
        cout << i+1 sq;
    }
    cout nl;
}
