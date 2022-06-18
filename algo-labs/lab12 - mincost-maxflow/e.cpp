#include <algorithm>
#include <iostream>
#include <iomanip>
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
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

struct edge {
    int from;
    int to;
    int capacity;
    int cost;
    int f;
    size_t backtrace;
    int num;
};

const ll INF = 1e17;

vector<vector<edge>> g;
vector<ll> potentials;
vector<int> used;
int n, m, k;

void add_edge(int a, int b, int u, int c, int i) {
    edge e1({a, b, u, c, 0, g[b].size(), i});
    edge e2({b, a, 0, -c, 0, g[a].size(), i});
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
    fill(used.begin(), used.end(), false);
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

bool dfs(int v, vector<vector<pair<int, pair<ll, int>>>> & g, vector<int> & path, ll & time) {
    if (v == n - 1) {
        return true;
    }

    for (auto [i, j]: g[v]) {
        auto [cost, num] = j;
        if (!used[num]) {
            used[num] = true;
            if (dfs(i, g, path, time)) {
                path.push_back(num);
                time += cost;
                return true;
            }
        }
    }

    return false;
}

void solve() {
    cin >> n >> m >> k;

    g.resize(n);
    potentials.resize(n);
    used.resize(n);

    for (int i = 0; i < m; ++i) {
        int a, b, c;
        cin >> a >> b >> c;
        a--, b--;
        add_edge(a, b, 1, c, i);
        add_edge(b, a, 1, c, i);
    }

    fordBellmanDists(potentials, 0);

    vector<ll> d(n);
    vector<pair<int, int>> p(n);

    ll max_flow = 0;
    ll min_cost = 0;

    while (max_flow < k) {
        //cout << "iter!" << endl;
        dijkstra(d, potentials, p, 0);
        //cout << "dijkstra done!" << endl;

        if (p[n - 1].first == -1) {
            break;
        }

        ll f = 1;

        // update path
        for (int v = n - 1; v != 0; v = p[v].first) {
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
        for (int i = 0; i < n; ++i) {
            if (d[i] != INF) {
                potentials[i] += d[i];
            }
        }

        //cout << "max flow to add found! " << max_flow << " " << min_cost << endl;
    }

    //cout << min_cost << endl;

    vector<vector<pair<int, pair<ll, int>>>> g2(n);
    int edges = 0;

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < g[i].size(); ++j) {
            edge & e = g[i][j];
            if (e.cost < 0) continue; // skip inv edges
            if (e.f == 1) {
                // edge is chosen
                g2[i].push_back({e.to, {e.cost, e.num}});
                ++edges;
            }
        }
    }

    /*for (int i = 0; i < n; ++i) {
        cout << "i " << i+1 << endl;
        for (int j = 0; j < g2[i].size(); ++j) {
            cout << g2[i][j].first+1 << " (" << g2[i][j].second.second << ") ";
        }
        cout << endl;
    }
    cout << endl;*/

    used.resize(m);
    fill(used.begin(), used.end(), false);

    vector<vector<int>> paths(k);
    vector<ll> times(k);

    for (int i = 0; i < k; ++i) {
        if (!dfs(0, g2, paths[i], times[i])) {
            cout << -1 << endl;
            return;
        }
        reverse(paths[i].begin(), paths[i].end());
    }

    long double average = 0;
    for (auto &i: times) average += i;

    cout << setprecision(6) << fixed << average/k << endl;

    for (int i = 0; i < k; ++i) {
        cout << paths[i].size() << " ";
        for (auto &j: paths[i]) {
            cout << j+1 << " ";
        }
        cout << endl;
    }
}
