#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <vector>
#include <deque>
#include <set>

using namespace std;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

vector<vector<int>> g;
vector<int> used, available_colors;
int n, m, k = 0;

void solve() {
    cin >> n >> m;
    g.resize(n);
    used.resize(n);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        a--, b--;
        g[a].push_back(b);
        g[b].push_back(a);
        k = max(k, (int) max(g[a].size(), g[b].size()));
    }

    k = (k & 1 ? k : k + 1);

    available_colors.resize(k + 1);
    int max_deg = 0;
    int start = 0;

    for (int i = 0; i < n; ++i) {
        if (max_deg < g[i].size()) {
            max_deg = g[i].size();
            start = i;
        }
    }

    deque<int> q{start};

    while (!q.empty()) {
        int v = q.front();
        q.pop_front();

        if (used[v]) {
            continue;
        }

        for (auto &i: g[v]) {
            if (used[i]) {
                available_colors[used[i]] = 1;
            }
        }

        int min_col = 0;
        for (int i = 1; i <= k; ++i) {
            if (!available_colors[i]) {
                min_col = i;
                break;
            }
        }

        used[v] = min_col;

        for (auto &i: g[v]) {
            if (!used[i]) {
                q.push_back(i);
            }
        }

        fill(available_colors.begin(), available_colors.end(), 0);
    }

    cout << k << endl;

    for (int i = 0; i < n; ++i) {
        cout << used[i] << endl;
    }
}
