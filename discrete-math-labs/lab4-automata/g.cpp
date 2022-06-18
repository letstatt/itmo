#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef long long ll;

void solve();

int main() {
    freopen("equivalence.in", "r", stdin);
    freopen("equivalence.out", "w", stdout);
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, int> d;
    bool accepts = false;
};

vector<vertex> g1, g2;

void closure(int v, vector<vertex> & g) {
    for (int i = 0; i < g.size(); ++i) {
        for (char c = 'a'; c <= 'z'; ++c) {
            if (!g[i].d.count(c)) {
                g[i].d[c] = g.size() - 1;
            }
        }
    }
}

void read_dfa(vector<vertex> &g) {
    int n, m, k;
    cin >> n >> m >> k;

    g.resize(n + 1);

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

    closure(0, g);
}

void solve() {
    read_dfa(g1);
    read_dfa(g2);

    set<pair<int, int>> used;
    queue<pair<int, int>> q;
    q.push({0, 0});

    while (!q.empty()) {
        auto [v, u] = q.front();
        q.pop();

        if (g1[v].accepts != g2[u].accepts) {
            cout << "NO" << endl;
            return;
        }

        used.insert({v, u});

        for (char c = 'a'; c <= 'z'; ++c) {
            int x = g1[v].d[c];
            int y = g2[u].d[c];
            if (!used.count({x, y})) {
                q.push({x, y});
            }
        }
    }

    cout << "YES" << endl;
}
