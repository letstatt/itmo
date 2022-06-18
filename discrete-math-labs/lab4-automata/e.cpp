#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef long long ll;

void solve();

int main() {
    freopen("problem5.in", "r", stdin);
    freopen("problem5.out", "w", stdout);
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, vector<int>> d;
    bool accepts = false;
    bool accessibility = false;
};

const ll mod = 1e9 + 7;
vector<vertex> g;
int n, m, k, l;

void thompson() {
    vector<vertex> dfa(200);
    map<set<int>, int> dict;
    queue<set<int>> q;
    q.push({0});
    dict[q.front()] = 0;
    dfa[0].accepts = g[0].accepts;

    while (!q.empty()) {
        auto s = q.front();
        q.pop();

        for (char c = 'a'; c <= 'z'; ++c) {
            set<int> t;
            bool accepts = false;

            for (auto &i: s) {
                for (auto &j: g[i].d[c]) {
                    t.insert(j);
                    accepts |= (g[j].accepts);
                }
            }
            if (!t.empty()) {
                if (dict.find(t) == dict.end()) {
                    dfa[dict.size()].accepts = accepts;
                    dict[t] = dict.size();
                    q.push(t);
                }
                dfa[dict[s]].d[c].push_back(dict[t]);
            }
        }
    }
    swap(g, dfa);
}

void topsort(int v, vector<int> &used, vector<int> &ts) {
    used[v] = 1;
    for (auto &i: g[v].d) {
        for (auto &j: i.second) {
            if (!used[j]) {
                topsort(j, used, ts);
            }
        }
    }
    ts.push_back(v);
    used[v] = 2;
}

void solve() {
    cin >> n >> m >> k >> l;

    g.resize(n);

    for (int i = 0, j; i < k; ++i) {
        cin >> j;
        g[j - 1].accepts = true;
    }

    for (int i = 0, a, b; i < m; ++i) {
        char c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].d[c].push_back(b);
    }

    thompson();

    vector<int> used(g.size());
    vector<int> ts;

    topsort(0, used, ts);

    reverse(ts.begin(), ts.end());
    vector<vector<ll>> d(l + 1, vector<ll>(g.size()));
    d[0][0] = 1;

    for (int k = 0; k < l; ++k) {
        for (auto &i: ts) {
            for (auto &j: g[i].d) {
                assert(j.second.size() == 1);
                for (auto &h: j.second) {
                    d[k + 1][h] += d[k][i];
                    d[k + 1][h] %= mod;
                }
            }
        }
    }

    ll res = 0;

    for (int i = 0; i < g.size(); ++i) {
        if (g[i].accepts) {
            res += d[l][i];
            res %= mod;
        }
    }

    cout << res << endl;
}
