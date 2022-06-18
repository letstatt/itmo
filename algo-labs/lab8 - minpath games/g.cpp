#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

using namespace std;

typedef unsigned long long ull;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

const ull INF = 1e18 + 1e17;

vector<vector<pair<short, ull>>> g;
short n, m;

struct dijkstra_entry {
    ull dist;
    ull mod; // rubbish, but needed to create virtual vertex
    short v;

    bool operator< (dijkstra_entry const& other) const {
        return (dist == other.dist ? make_pair(mod, v) < make_pair(other.mod, other.v) : dist < other.dist);
    }
};

void solve() {
    cin >> n >> m;
    g.resize(n);

    for (short i = 0; i < m; ++i) {
        short x, y;
        ull w;
        cin >> x >> y >> w;
        x--, y--;
        g[x].push_back({y, w});
        g[y].push_back({x, w});
    }

    if (g[n-1].empty()) {
        cout << "Impossible" nl;
        return;
    }

    ull mod = INF;

    for (auto &i: g[n-1]) {
        mod = min(mod, i.second);
    }
    mod *= 2;

    ull t;
    cin >> t;
    ull t_mod = t % mod;

    vector<vector<ull>> d(n, vector<ull>(mod, INF));
    set<dijkstra_entry> q;

    d[0][0] = 0;
    q.insert({0, mod, 0});

    while (!q.empty()) {
        auto [dist, _, v] = *q.begin();
        q.erase(q.begin());

        for (auto [j, w]: g[v]) {
            ull _mod = (dist + w) % mod;

            if (d[j][_mod] > dist + w) {
                q.erase({d[j][_mod], _mod, j});
                d[j][_mod] = dist + w;
                q.insert({d[j][_mod], _mod, j});
            }

            if (j == n-1 && _mod == t_mod && d[j][t_mod] <= t) {
                cout << "Possible" nl;
                return;
            }
        }
    }

    cout << "Impossible" nl;

}
