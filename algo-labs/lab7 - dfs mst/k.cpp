#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
//#define nl << '\n'
#define nl << endl
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<vector<pair<short, ll>>> g;
vector<bool> used;
int n, m;

struct edge {
    short a, b;
    int w;
};

void precheck(short i) {
    used[i] = 1;
    for (auto &e: g[i]) {
        auto [j, w] = e;
        if (!used[j]) {
            precheck(j);
        }
    }
}

void dfs(short i, vector<vector<short>>& g, vector<bool>& used) {
    used[i] = 1;
    for (auto &j: g[i]) {
        if (!used[j]) {
            dfs(j, g, used);
        }
    }
}

void topsort(short i, vector<vector<short>>& g, vector<bool>& used, vector<short> &order) {
    used[i] = 1;
    for (auto &j: g[i]) {
        if (!used[j]) {
            topsort(j, g, used, order);
        }
    }
    order.push_back(i);
}

void mark_comp(short i, vector<vector<short>>& gr, vector<short>& comp, short c) {
    comp[i] = c;
    for (auto &j: gr[i]) {
        if (comp[j] == -1) {
            mark_comp(j, gr, comp, c);
        }
    }
}

bool check(short root, vector<edge>& edges, vector<short>& comp) {
    vector<vector<short>> g, gr;
    short n = 0;

    for (auto &i: edges) {
        n = max(n, short(max(i.a, i.b) + 1));
    }

    g.resize(n);
    gr.resize(n);
    comp.resize(n, -1);
    vector<bool> used(n);

    for (auto &i: edges) {
        g[i.a].push_back(i.b);
        gr[i.b].push_back(i.a);
    }

    //cout << "check#1" nl;

    dfs(root, g, used);
    //cout << "check#1" nl;
    if (accumulate(used.begin(), used.end(), 0) == n) {
        return true;
    }

    fill(used.begin(), used.end(), 0);
    vector<short> order;

    //cout << "check#1" nl;

    for (short i = 0; i < n; ++i) {
        if (!used[i]) {
            topsort(i, g, used, order);
        }
    }

    g.clear();

    //cout << "check#1" nl;

    reverse(order.begin(), order.end());
    short c = -1;
    for (auto &i: order) {
        if (comp[i] == -1) {
            c++;
            mark_comp(i, gr, comp, c);
        }
    }
    return false;
}

ll findMST(vector<edge> edges, short n, short root) {
    ll res = 0;
    vector<ll> min_edge(n, 1e15);

    if (n == 1) {
        return 0;
    }

    //cout << "iter:" sp n nl;

    for (auto &e: edges) {
        min_edge[e.b] = min((ll)e.w, min_edge[e.b]);
    }

    for (short i = 0; i < n; ++i) {
        if (i != root) {
            res += min_edge[i];
        }
    }

    vector<edge> zero_edges;

    for (auto &e: edges) {
        if (e.w == min_edge[e.b]) {
            zero_edges.push_back({e.a, e.b, /*(ll)e.w - min_edge[e.b]*/ 0}); // ?
        }
    }

    vector<short> comp;

    if (check(root, zero_edges, comp)) {
        return res;
    }

    vector<edge> new_edges;

    for (auto &e: edges) {
        if (comp[e.a] != comp[e.b]) {
            new_edges.push_back({comp[e.a], comp[e.b], (ll)e.w - min_edge[e.b]}); // bad cast ll to int
        }
    }

    edges.clear();
    min_edge.clear();
    zero_edges.clear();

    short componentsCount = 0;
    for (auto &i: comp) {
        componentsCount = max(componentsCount, short(i + 1));
    }

    res += findMST(new_edges, componentsCount, comp[root]);
    return res;
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    used.resize(n);
    vector<edge> edges;

    for (short i = 0; i < m; ++i) {
        short a, b;
        int c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].push_back({b, c});
        edges.push_back({a, b, c});
    }

    precheck(0);

    if (accumulate(used.begin(), used.end(), 0) != n) {
        cout << "NO" nl;
        return;
    }

    g.clear();

    cout << "YES" nl;
    cout << findMST(edges, n, 0) nl;

}

