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
vector<int> used, tin, tup, buf;
vector<int> comp;
int n, m, t = 0, c = 0;

void dfs(int v, int p) {
    used[v] = 1;
    tin[v] = tup[v] = t++;
    buf.push_back(v);

    int to_parent = 0;

    for (auto &i: g[v]) {
        if (i == p) {
            to_parent++;
            if (to_parent > 1) {
                tup[v] = min(tup[v], tin[i]);
            }
            continue;
        }

        if (used[i]) {
            tup[v] = min(tup[v], tin[i]);
        } else {
            dfs(i, v);
            tup[v] = min(tup[v], tup[i]);
        }
    }

    if (tup[v] == tin[v]) {
        while (true) {
            int x = buf.back();
            comp[x] = c;
            buf.pop_back();
            if (x == v) {
                break;
            }
        }
        c++;
    }
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    used.resize(n);
    tin.resize(n);
    tup.resize(n);
    comp.resize(n);

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

    cout << c nl;
    for (auto &i: comp) {
        cout << i+1 sq;
    }
    cout nl;
}
