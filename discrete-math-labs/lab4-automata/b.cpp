#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>

using namespace std;

void solve();

int main() {
    freopen("problem2.in", "r", stdin);
    freopen("problem2.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, vector<int>> d;
    bool accepts = false;
};

vector<vertex> g;
int n, m, k;

bool bfs(string s) {
    unordered_set<int> q{0};
    unordered_set<int> t;

    for (auto &i: s) {
        for (auto &j: q) {
            t.insert(g[j].d[i].begin(), g[j].d[i].end());
        }
        q.clear();
        swap(q, t);
    }

    for (auto &i: q) {
        if (g[i].accepts) {
            return true;
        }
    }

    return false;
}

void solve() {
    string s;
    cin >> s >> n >> m >> k;

    g.resize(n);

    for (int i = 0, j; i < k; ++i) {
        cin >> j;
        g[j - 1].accepts = true;
    }

    for (int i = 0, a, b; i < m; ++i) {
        char c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].d[c].push_back(b);
    }

    cout << (bfs(s) ? "Accepts" : "Rejects") << endl;
}
