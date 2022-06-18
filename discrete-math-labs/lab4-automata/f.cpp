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
    freopen("isomorphism.in", "r", stdin);
    freopen("isomorphism.out", "w", stdout);
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, int> d;
    bool accepts = false;
};

vector<vertex> g1, g2;

void read_dfa(vector<vertex> &g) {
    int n, m, k;
    cin >> n >> m >> k;

    g.resize(n);

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
}

void solve() {
    read_dfa(g1);
    read_dfa(g2);

    //vector<vector<int>> used(g1.size(), vector<int>(g2.size()));
    set<pair<int, int>> used;
    queue<pair<int, int>> q;
    q.push({0, 0});

    if (g1[0].accepts != g2[0].accepts) {
        cout << "NO" << endl;
        return;
    }

    while (!q.empty()) {
        auto [v, u] = q.front();
        q.pop();

        for (char c = 'a'; c <= 'z'; ++c) {
            if (g1[v].d.count(c) && g2[u].d.count(c)) {
                int x = g1[v].d[c];
                int y = g2[u].d[c];
                if (!used.count({x, y})) {
                    if (g1[x].accepts != g2[y].accepts) {
                        cout << "NO" << endl;
                        return;
                    }
                    used.insert({x, y});
                    q.push({x, y});
                }
            } else if (g1[v].d.count(c) || g2[u].d.count(c)) {
                cout << "NO" << endl;
                return;
            }
        }
    }

    cout << "YES" << endl;
}
