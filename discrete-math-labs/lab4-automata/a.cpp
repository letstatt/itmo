#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>

using namespace std;

void solve();

int main() {
    freopen("problem1.in", "r", stdin);
    freopen("problem1.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

struct vertex {
    unordered_map<char, vector<int>> d;
    bool accepts = false;
};

vector<vertex> g;
int n, m, k;

bool dfs(string s, int v, int stack_protector = 1e6) {
    if (s.empty()) {
        return (g[v].accepts);
    }
    for (auto &i: g[v].d[s[0]]) {
        if (dfs(s.substr(1), i, stack_protector - 1)) {
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

    cout << (dfs(s, 0) ? "Accepts" : "Rejects") << endl;
}
