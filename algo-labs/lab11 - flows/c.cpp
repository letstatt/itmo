#include <unordered_map>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <queue>
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

const ll INF = 1e10;

struct edge {
    int a, b;
    ll c = 0, f = 0;
};

int n, s, t, m;
vector<vector<int>> g; // dinic
vector<edge> e; // dinic
vector<int> p; // dfs
vector<int> d; // bfs

void add_edge(int a, int b, ll c) {
    g[a].push_back(e.size());
    e.push_back({a, b, c, 0ll});
    g[b].push_back(e.size());
    e.push_back({b, a, 0ll, 0ll});
}

bool bfs() {
    fill(d.begin(), d.end(), -1);
    d[s] = 0;

    queue<int> q({s});

    while (!q.empty() && d[t] == -1) {
        int v = q.front();
        q.pop();

        for (auto &id: g[v]) {
            int to = e[id].b;
            if (d[to] == -1 && e[id].f < e[id].c) {
                d[to] = d[v] + 1;
                q.push(to);
            }
        }
    }
    return d[t] != -1;
}

ll dfs(int v, ll flow) {
    if (v == t || flow == 0) {
        return flow;
    }
    for (; p[v] < g[v].size(); ++p[v]) {
        int id = g[v][p[v]], to = e[id].b;
        if (d[to] != d[v] + 1) {
            continue;
        }
        int pushed = dfs(to, min(flow, e[id].c - e[id].f));
        if (pushed) {
            e[id].f += pushed;
            e[id^1].f -= pushed;
            return pushed;
        }
    }
    return 0;
}

ll dinic() {
    ll flow = 0;
    while (bfs()) {
        fill(p.begin(), p.end(), 0);
        while (int pushed = dfs(s, INF)) {
            flow += pushed;
        }
    }
    return flow;
}

void dfs2(int v, vector<int>& p) {
    p.push_back(v);

    if (v == t) {
        return;
    }

    for (auto &id: g[v]) {
        int to = e[id].b;
        if (e[id].f != e[id].c || e[id].c == 0) {
            continue; // non-saturated or empty pipe
        }
        e[id].f = e[id^1].f = 0;
        dfs2(to, p);
        break;
    }
}

void solve() {
    cin >> n >> m >> s >> t;
    s--, t--;

    g.resize(n);
    d.resize(n);
    p.resize(n);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        a--, b--;
        add_edge(a, b, 1);
        //add_edge(b, a, c);
    }

    ll flow = dinic();

    if (flow < 2) {
        cout << "NO" nl;
        return;
    }

    cout << "YES" nl;

    vector<int> p1, p2;

    dfs2(s, p1);
    dfs2(s, p2);

    for (auto &i: p1) {
        cout << i+1 sq;
    }
    cout nl;

    for (auto &i: p2) {
        cout << i+1 sq;
    }
    cout nl;
}
