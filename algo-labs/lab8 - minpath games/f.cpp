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

vector<vector<pair<int, ll>>> g;
vector<ll> d;
int n, m;

ll dijkstra(int s, int t) {
    fill(d.begin(), d.end(), INF);
    set<pair<int, int>> q;
    q.insert({0, s});
    d[s] = 0;

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
    return d[t];
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    d.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y, w;
        cin >> x >> y >> w;
        x--, y--;
        g[x].push_back({y, w});
        g[y].push_back({x, w});
    }

    int a, b, c;
    cin >> a >> b >> c;
    a--, b--, c--;

    ll ab = dijkstra(a, b);
    ll ac = dijkstra(a, c);
    ll bc = dijkstra(b, c);

    set<ll> paths;

    if (ab != INF && bc != INF) {
        paths.insert(ab + bc);
    }

    if (ac != INF && bc != INF) {
        paths.insert(ac + bc);
    }

    if (ab != INF && ac != INF) {
        paths.insert(ab + ac);
    }

    cout << (paths.empty() ? -1 : *paths.begin()) nl;
}
