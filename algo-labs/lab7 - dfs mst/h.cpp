#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <cmath>

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

vector<vector<pair<int, int>>> g, gr;
vector<int> used, order;
int n;

void topsort(int i, int max_cost) {
    used[i] = 1;
    for (auto &e: g[i]) {
        auto [j, cost] = e;
        if (!used[j] && cost <= max_cost) {
            topsort(j, max_cost);
        }
    }
    order.push_back(i);
}

void dfs(int i, int max_cost) {
    used[i] = 1;
    for (auto &e: gr[i]) {
        auto [j, cost] = e;
        if (!used[j] && cost <= max_cost) {
            dfs(j, max_cost);
        }
    }
}

bool check(int max_cost) {
    fill(used.begin(), used.end(), 0);
    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            topsort(i, max_cost);
        }
    }

    fill(used.begin(), used.end(), 0);
    dfs(order.back(), max_cost);
    order.clear();

    return accumulate(used.begin(), used.end(), 0) == n;
}

void solve() {
    cin >> n;
    g.resize(n);
    gr.resize(n);
    used.resize(n);

    for (int i = 0; i < n; ++i) {
        for (int j = 0, x; j < n; ++j) {
            cin >> x;
            if (i == j) {
                continue;
            } else {
                g[i].push_back({j, x});
                gr[j].push_back({i, x});
            }
        }
    }

    ll l = -1, r = 1e9; // l excluded

    while (r - l > 1) {
        ll m = (l + r) / 2;
        if (!check(m)) {
            l = m;
        } else {
            r = m;
        }
    }

    cout << r nl;
}
