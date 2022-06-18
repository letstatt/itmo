#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <iostream>
#include <cassert>
#include <vector>
#include <queue>
#include <cmath>
#include <set>
#include <map>

using namespace std;

typedef long long ll;
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

struct node {
    char c = '#';
    int parent = -1;
    int link = -1;
    bool terminal = false;
    unordered_set<int> ids;
    unordered_map<char, int> m;
};

vector<node> corasick;

string t;
int m;

void add_string(string& s, int id) {
    int ptr = 0;
    for (auto &c: s) {
        if (!corasick[ptr].m.count(c)) {
            corasick[ptr].m[c] = corasick.size();
            corasick.push_back({c, ptr});
        }
        ptr = corasick[ptr].m[c];
    }
    corasick[ptr].terminal = true;
    corasick[ptr].ids.insert(id);
}

void add_link(int ptr) {
    int link = corasick[corasick[ptr].parent].link;
    while (link != -1 && !corasick[link].m.count(corasick[ptr].c)) {
        link = corasick[link].link;
    }
    corasick[ptr].link = (link == -1 ? 0 : corasick[link].m[corasick[ptr].c]);
}

int go(int ptr, char c) {
    while (ptr != -1 && !corasick[ptr].m.count(c)) {
        ptr = corasick[ptr].link;
    }
    return (ptr == -1 ? 0 : corasick[ptr].m[c]);
}

void build() {
    queue<int> q;
    for (auto &i: corasick[0].m) {
        q.push(i.second);
    }
    while (!q.empty()) {
        int v = q.front();
        add_link(v);
        for (auto &i: corasick[v].m) {
            q.push(i.second);
        }
        q.pop();
    }
}

void dfs(int v, vector<vector<int>>& g, vector<int>& sum) {
    for (auto &i: g[v]) {
        dfs(i, g, sum);
        sum[v] += sum[i];
    }
}

void solve() {
    corasick.reserve(1000000);
    corasick.emplace_back();

    cin >> m;
    for (int i = 0; i < m; ++i) {
        string s;
        cin >> s;
        add_string(s, i);
    }

    build();
    cin >> t;

    vector<int> used(corasick.size());
    unordered_map<int, int> ans;
    int ptr = 0;

    for (auto &c: t) {
        ptr = go(ptr, c);
        ++used[ptr];
    }

    vector<vector<int>> tree(corasick.size());

    for (int i = 1; i < corasick.size(); ++i) {
        if (corasick[i].link == -1) {
            add_link(i);
        }
        tree[corasick[i].link].push_back(i);
    }

    dfs(0, tree, used);

    for (int i = 1; i < used.size(); ++i) {
        if (corasick[i].terminal && used[i]) {
            for (auto &j: corasick[i].ids) {
                ans[j] += used[i];
            }
        }
    }

    for (int i = 0; i < m; ++i) {
        cout << ans[i] nl;
    }
}
