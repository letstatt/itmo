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
typedef long double ld;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

int m, k, n, t, q;
vector<vector<int>> g;
vector<int> used;
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

void solve() {
    cin >> m >> k >> n >> t;

    int N = m + k - n;

    g.resize(N);
    used.resize(N);
    matching.resize(N, -1);

    set<pair<int, int>> conflicts;

    for (int i = 0, x, y; i < t; ++i) {
        cin >> x >> y;
        x--, y--;
        conflicts.insert({x, y});
        conflicts.insert({y, x});
    }

    cin >> q;

    set<int> not_alone;

    for (int i = 0, j; i < q; ++i) {
        cin >> j;
        not_alone.insert(j - 1);
    }

    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < N; ++j) {
            bool ghost_left = i >= m;
            bool ghost_right = j >= k;

            if (ghost_left && ghost_right) {
                continue;
            }

            if (ghost_left && !ghost_right && not_alone.count(j + m)) {
                continue;
            }

            if (ghost_right && !ghost_left && not_alone.count(i)) {
                continue;
            }

            if (!ghost_left && !ghost_right && conflicts.count({i, j + m})) {
                continue;
            }

            g[i].push_back(j);
        }
    }

    for (int i = 0; i < N; ++i) {
        kuhn(i);
        fill(used.begin(), used.end(), 0);
    }

    for (auto &i: matching) {
        if (i == -1) {
            cout << "NO" nl;
            return;
        }
    }

    cout << "YES" nl;

    for (int i = 0; i < k; ++i) {
        if (matching[i] < m) {
            cout << matching[i]+1 sp m+i+1 nl;
        }
    }
}

