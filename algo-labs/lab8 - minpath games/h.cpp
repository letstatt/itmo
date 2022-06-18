#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

const ll INF = 1e17;

vector<vector<int>> g;
vector<int> used, order;
vector<int> d;
int n, m, s;

void dfs(int i) {
    used[i] = 1;
    for (auto &j: g[i]) {
        if (!used[j]) {
            dfs(j);
        }
    }
    order.push_back(i);
}

void solve() {
    cin >> n >> m >> s;
    g.resize(n);
    d.resize(n);
    used.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back(y);
    }

    dfs(s-1);

    for (auto &i: order) {
        for (auto &j: g[i]) {
            if (d[i] || !d[j]) { // if i win, i have a winning strategy
                d[i] = 1;
            }
        }
    }

    cout << (d[s-1] ? "First" : "Second") sp "player wins" nl;
}
