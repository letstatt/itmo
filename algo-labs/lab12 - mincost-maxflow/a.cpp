#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

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

struct edge {
    int from;
    int to;
    int capacity;
    int cost;
    int f;
    size_t backtrace;
};

const ll INF = 1e17;

vector<vector<edge>> g;
vector<ll> potentials;

int n, m;

void add_edge(int a, int b, int u, int c) {
    edge e1({a, b, u, c, 0, g[b].size()});
    edge e2({b, a, 0, -c, 0, g[a].size()});
    g[a].push_back(e1);
    g[b].push_back(e2);
}

void fordBellmanDists(vector<ll> & d, int s) {
    fill(d.begin(), d.end(), INF);
    d[s] = 0;

    while (true) {
        bool any = false;
        for (auto const& i: g) {
            for (auto const& j: i) {
                if (j.f == j.capacity) {
                    continue;
                }
                if (d[j.to] > d[j.from] + j.cost && j.from != INF) {
                    d[j.to] = d[j.from] + j.cost;
                    any = true;
                }
            }
        }

        if (!any) {
            break;
        }
    }
}

void dijkstra(vector<ll> & d, vector<ll> & potentials, vector<pair<int, int>> & path, int s) {
    fill(d.begin(), d.end(), INF);
    fill(path.begin(), path.end(), make_pair(-1, -1));
    vector<int> used(n, false);
    d[s] = 0;

    for (int i = 0; i < n; ++i) {
        int v = -1;
        for (int j = 0; j < n; ++j) {
            if (!used[j] && d[j] != INF && (v == -1 || d[j] < d[v])) {
                v = j;
            }
        }
        if (v == -1) {
            break;
        }
        used[v] = true;

        for (int j = 0; j < g[v].size(); ++j) {
            int to = g[v][j].to;
            ll len = g[v][j].cost + potentials[v] - potentials[to];
            if (g[v][j].f < g[v][j].capacity && d[v] + len < d[to]) {
                d[to] = d[v] + len;
                path[to] = {v, j};
            }
        }
    }
}

void solve() {
    cin >> n >> m;

    g.resize(n);
    potentials.resize(n);

    for (int i = 0, a, b, u, c; i < m; ++i) {
        cin >> a >> b >> u >> c;
        a--, b--;
        add_edge(a, b, u, c); // oriented graph
    }

    fordBellmanDists(potentials, 0);

    vector<ll> d(n);
    vector<pair<int, int>> p(n);

    ll max_flow = 0;
    ll min_cost = 0;

    //cout << "potentials found" << endl;

    while (true) {
        //cout << "iter!" << endl;
        dijkstra(d, potentials, p, 0);
        //cout << "dijkstra done!" << endl;

        if (p[n - 1].first == -1) {
            break;
        }

        // find max flow to add
        ll f = INF;
        for (int v = n - 1; v != 0; v = p[v].first) {
            //cout << "edge " << p[v].first+1 << " " << v+1 << endl;
            edge & e = g[p[v].first][p[v].second];
            f = min(f, (ll) e.capacity - e.f);
        }

        //cout << "max flow to add found!" << endl;

        // update path
        for (int v = n - 1; v != 0; v = p[v].first) {
            edge & e = g[p[v].first][p[v].second];
            edge & e_inv = g[e.to][e.backtrace];
            e.f += f;
            e_inv.f -= f;
            min_cost += f * e.cost;
        }
        max_flow += f;

        //cout << "path updated!" << endl;

        // update potentials
        for (int i = 0; i < n; ++i) {
            if (d[i] != INF) {
                potentials[i] += d[i];
            }
        }

        //cout << "max flow to add found! " << max_flow << " " << min_cost << endl;
    }

    cout << min_cost << endl;
}
