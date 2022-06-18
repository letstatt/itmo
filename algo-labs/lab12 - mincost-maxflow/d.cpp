#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

typedef long long ll;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

const ll INF = 1e16;

struct edge {
    int from;
    int to;
    ll capacity;
    ll cost;
    ll f;
    size_t backtrace;
};

int n, m;
vector<vector<ll>> f;
vector<ll> staticPrice;
vector<vector<edge>> g;
vector<ll> potentials;

void add_edge(int a, int b, ll u, ll c) {
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
    vector<int> used(2 * n + 2, false);
    d[s] = 0;

    for (int i = 0; i < 2 * n + 2; ++i) {
        int v = -1;
        for (int j = 0; j < 2 * n + 2; ++j) {
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
    f.resize(n, vector<ll>(n, INF));
    g.resize(2 * n + 2);
    potentials.resize(2 * n + 2);
    staticPrice.resize(n);

    for (auto &i: staticPrice) cin >> i;

    for (int i = 0, a, b; i < m; ++i) {
        cin >> a >> b;
        a--, b--;
        cin >> f[a][b];
    }


    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (f[i][k] < INF && f[k][j] < INF) {
                    f[i][j] = min(f[i][j], f[i][k] + f[k][j]);
                }
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        f[i][i] = min(f[i][i], staticPrice[i]);
    }

    /*for (auto &i: f) {
        for (auto &j: i) cout << j << " ";
        cout << endl;
    }
    cout << endl;*/

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            add_edge(i + 1, j + n + 1, 1, f[i][j]);
        }
        add_edge(0, i + 1, 1, 0);
        add_edge(i + n + 1, 2 * n + 1, 1, 0);
    }

    fordBellmanDists(potentials, 0);

    vector<ll> d(2 * n + 2);
    vector<pair<int, int>> p(2 * n + 2);

    ll max_flow = 0;
    ll min_cost = 0;

    while (true) {
        //cout << "iter!" << endl;
        dijkstra(d, potentials, p, 0);
        //cout << "dijkstra done!" << endl;

        if (p[2 * n + 1].first == -1) {
            break;
        }

        ll f = 1;

        // update path
        for (int v = 2 * n + 1; v != 0; v = p[v].first) {
            //cout << "edge " << p[v].first << " " << v << endl;
            edge & e = g[p[v].first][p[v].second];
            edge & e_inv = g[e.to][e.backtrace];
            e.f += f;
            e_inv.f -= f;
            min_cost += f * e.cost;
        }
        max_flow += f;

        //cout << "path updated!" << endl;

        // update potentials
        for (int i = 0; i < 2 * n + 2; ++i) {
            if (d[i] != INF) {
                potentials[i] += d[i];
            }
        }

        //cout << "max flow to add found! " << max_flow << " " << min_cost << endl;
    }

    cout << min_cost << endl;


}
