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

vector<vector<int>> g, gr;
vector<set<int>> condensed;
vector<int> used, order, comp;
int n, m, c = 0;


void topsort(int v) {
    used[v] = 1;
    for (auto &i: g[v]) {
        if (!used[i]) {
            topsort(i);
        }
    }
    order.push_back(v);
}

void dfs(int v) {
    comp[v] = c;
    for (auto &i: gr[v]) {
        if (!comp[i]) {
            dfs(i);
        }
    }
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    gr.resize(n);
    used.resize(n);
    comp.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back(y);
        gr[y].push_back(x);
    }

    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            topsort(i);
        }
    }

    reverse(order.begin(), order.end());

    for (auto &i: order) {
        if (!comp[i]) {
            c++;
            dfs(i);
        }
    }

    condensed.resize(c + 1);

    for (int i = 0; i < n; ++i) {
        for (auto &j: g[i]) {
            if (comp[i] != comp[j]) {
                condensed[comp[i]].insert(comp[j]);
            }
        }
    }

    int ans = 0;

    for (auto &i: condensed) {
        ans += i.size();
    }

    cout << ans nl;
}
