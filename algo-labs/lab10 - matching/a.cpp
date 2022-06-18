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
    solve();
}

int n, m;
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
    cin >> n >> m;

    g.resize(n);
    used.resize(n);
    matching.resize(m, -1);

    for (int i = 0, j; i < n; ++i) {
        while ((cin >> j), j != 0) {
            g[i].push_back(j - 1);
        }
    }

    for (int i = 0; i < n; ++i) {
        if (kuhn(i)) {
            fill(used.begin(), used.end(), 0);
        }
    }

    int ans = 0;

    for (auto &i: matching) {
        ans += (i != -1);
    }

    cout << ans nl;
    for (int i = 0; i < m; ++i) {
        if (matching[i] != -1) {
            cout << matching[i]+1 sp i+1 nl;
        }
    }
    cout << endl;
}
