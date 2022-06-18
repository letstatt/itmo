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
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<vector<pair<int, int>>> g;
vector<int> d;
int n, m;

void solve() {
    cin >> n >> m;
    g.resize(n);
    d.resize(n, 1e9);

    for (int i = 0; i < m; ++i) {
        int x, y, w;
        cin >> x >> y >> w;
        x--, y--;
        g[x].push_back({y, w});
        g[y].push_back({x, w});
    }

    set<pair<int, int>> q;
    q.insert({0, 0});
    d[0] = 0;

    while (!q.empty()) {
        auto [dist, v] = *q.begin();
        q.erase(q.begin());

        for (auto [i, w]: g[v]) {
            if (d[i] > d[v] + w) {
                auto it = q.find({d[i], i});
                if (it != q.end()) {
                    q.erase(it);
                }
                d[i] = d[v] + w;
                q.insert({d[i], i});
            }
        }
    }

    for (auto &i: d) {
        cout << i sq;
    }
    cout nl;
}
