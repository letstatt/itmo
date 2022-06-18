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
    freopen("fastminimization.in", "r", stdin);
    freopen("fastminimization.out", "w", stdout);
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct vertex {
    unordered_map<char, vector<int>> d;
    bool accepts = false;
};

struct min_vertex {
    unordered_map<char, int> d;
    bool accepts = false;
};

vector<int> used;
vector<min_vertex> g;
vector<vertex> gr;
int n, m, k;

void dfs(int v) {
    used[v] = 1;
    for (auto &i: gr[v].d) {
        for (auto &j: i.second) {
            if (!used[j]) {
                dfs(j);
            }
        }
    }
}

void recalc(vector<min_vertex> &dfa, vector<min_vertex> &ans, map<int, int> &dict, int v) {
    int u = dict.size();

    ans[u].accepts = dfa[v].accepts;
    used[v] = 4;
    dict[v] = u;

    for (auto &i: dfa[v].d) {
        if (used[i.second] < 4) {
            recalc(dfa, ans, dict, i.second);
        }
        ans[u].d[i.first] = dict[i.second];
    }
}

void solve() {
    cin >> n >> m >> k;

    g.resize(n);
    gr.resize(n);
    used.resize(n);

    for (int i = 0, j; i < k; ++i) {
        cin >> j;
        g[j - 1].accepts = true;
    }

    for (int i = 0, a, b; i < m; ++i) {
        char c;
        cin >> a >> b >> c;
        a--, b--;
        g[a].d[c] = b;
        gr[b].d[c].push_back(a);
    }

    for (int i = 0; i < n; ++i) {
        if (g[i].accepts && !used[i]) {
            dfs(i);
        }
    }

    vector<int> classes(n);
    set<int> F;
    set<int> Q_without_F;

    for (int i = 0; i < n; ++i) {
        if (!used[i]) continue;
        if (g[i].accepts) {
            F.insert(i);
        } else {
            Q_without_F.insert(i);
            classes[i] = 1;
        }
    }

    // Hopcroft's Algorithm
    vector<set<int>> P{F, Q_without_F};
    queue<pair<int, char>> q;

    for (char c = 'a'; c <= 'z'; ++c) {
        q.push({0, c});
        q.push({1, c});
    }

    while (!q.empty()) {
        auto [Class, c] = q.front();
        q.pop();

        map<int, vector<int>> involved;

        for (auto &i: P[Class]) {
            for (auto &j: gr[i].d[c]) {
                involved[classes[j]].push_back(j);
            }
        }

        for (auto &i: involved) {
            if (!i.second.empty()) {
                if (involved[i.first].size() < P[i.first].size()) {
                    int j = P.size();
                    P.emplace_back();

                    for (auto &r: involved[i.first]) {
                        P[i.first].erase(r);
                        P[j].insert(r);
                    }
                    if (P[j].size() > P[i.first].size()) {
                        swap(P[j], P[i.first]);
                    }
                    for (auto &r: P[j]) {
                        classes[r] = j;
                    }
                    for (char c = 'a'; c <= 'z'; ++c) {
                        q.push({j, c});
                    }
                }
            }
        }
    }

    vector<min_vertex> dfa(P.size());

    for (int i = 0; i < n; ++i) {
        if (used[i]) {
            for (auto &j: g[i].d) {
                if (used[j.second]) {
                    dfa[classes[i]].d[j.first] = classes[j.second];
                }
            }
            dfa[classes[i]].accepts |= g[i].accepts;
        }
    }

    /*
2 2 2
1 2
1 2 a
2 2 a

2 2 2
1 2
1 2 a
2 2 b

1 1 1
1
1 1 a

1 2 1
1
1 1 a
1 1 b

3 3 1
3
1 2 a
2 3 b
1 3 c

4 4 2
1 3
1 2 0
2 3 0
3 4 0
4 1 0

4 8 2
1 3
1 1 1
2 2 1
3 3 1
4 4 1
1 2 0
2 3 0
3 4 0
4 1 0
     */

    vector<min_vertex> ans(P.size());
    map<int, int> dict; // нумерация вершин тупа

    recalc(dfa, ans, dict, classes[0]);

    ans.resize(dict.size());

    int edges = 0;
    for (auto &i: ans) {
        edges += i.d.size();
    }

    int terminals = 0;
    for (auto &i: ans) {
        terminals += int(i.accepts);
    }

    cout << ans.size() << " " << edges << " " << terminals << endl;

    for (int i = 0; i < ans.size(); ++i) {
        if (ans[i].accepts) {
            cout << i+1 << " ";
        }
    }
    cout << endl;

    for (int i = 0; i < ans.size(); ++i) {
        for (auto &j: ans[i].d) {
            cout << i+1 << " " << j.second+1 << " " << j.first << endl;
        }
    }
}
