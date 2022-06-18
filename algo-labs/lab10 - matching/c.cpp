#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    int k;
    cin >> k;
    while (k--) {
        solve();
    }
}

int n, m;
vector<vector<int>> g;
vector<int> used, used2;
vector<int> matching;

bool kuhn(int v) {
    if (used[v]) {
        return false;
    }

    used[v] = true;

    for (auto &i: g[v]) {
        if (matching[i] == -1 || kuhn(matching[i])) {
            matching[i] = v;
            return true;
        }
    }

    return false;
}

void is(int v) {
    //cout << "is " << v nl;
    if (used[v]) {
        return;
    }

    used[v] = true;

    for (auto &i: g[v]) {
        if (!used2[i]) {
            used2[i] = true;
            is(matching[i]);
        }
    }
}

void solve() {
    cin >> n >> m;

    g.clear();
    used.clear();
    used2.clear();
    matching.clear();

    g.resize(n);
    used.resize(n);
    used2.resize(m);
    matching.resize(m, -1);

    vector<unordered_set<int>> gr(n);

    for (int i = 0, j; i < n; ++i) {
        while ((cin >> j), j != 0) {
            gr[i].insert(j - 1);
        }
    }

    // find max bipartite graph clique:
    // 1. invert edges
    // 2. find max independent set

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (!gr[i].count(j)) {
                g[i].push_back(j);
            }
        }
    }

    // find max matching at first

    for (int i = 0; i < n; ++i) {
        if (kuhn(i)) {
            fill(used.begin(), used.end(), 0);
        }
    }

    // use matching to find max IS

    unordered_set<int> busy(matching.begin(), matching.end());

    fill(used.begin(), used.end(), 0);

    for (int i = 0; i < n; ++i) {
        if (!busy.count(i)) {
            is(i);
        }
    }

    set<int> ans1, ans2;

    for (int i = 0; i < n; ++i) {
        if (used[i]) {
            ans1.insert(i);
        }
    }

    for (int i = 0; i < m; ++i) {
        if (!used2[i]) {
            ans2.insert(i);
        }
    }

    cout << (ans1.size() + ans2.size()) nl;
    cout << ans1.size() sp ans2.size() nl;

    for (auto &i: ans1) {
        cout << i+1 sq;
    }
    cout nl;

    for (auto &i: ans2) {
        cout << i+1 sq;
    }
    cout nl;
}
