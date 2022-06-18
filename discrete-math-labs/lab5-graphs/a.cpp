#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie();
    cin.tie();
    solve();
}

vector<vector<int>> g;
vector<int> used;
vector<int> cycle;
int n;

void dfs(int i, int d) {
    if (used[i]) {
        return;
    }
    cycle.push_back(i);
    used[i] = true;
    if (d == n) {
        if (count(g[i].begin(), g[i].end(), cycle[0]) == 1) {
            for (auto &j: cycle) {
                cout << j+1 << " ";
            }
            cout << endl;
            exit(0);
        }
    } else {
        for (auto &j: g[i]) {
            dfs (j, d + 1);
        }
    }
    cycle.pop_back();
    used[i] = 0;
}

void solve() {
    cin >> n;
    g.resize(n);
    used.resize(n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char x;
            cin >> x;
            if (x == '1') {
                g[i].push_back(j);
                g[j].push_back(i);
            }
        }
    }
    int s = 0;

    for (int i = 0; i < n; ++i) {
        sort(g[i].begin(), g[i].end(), [](const int& a, const int& b) {
            return g[a].size() < g[b].size();
        });
        if (g[i].size() < g[s].size()) {
            s = i;
        }
    }

    dfs(s, 1);
}
