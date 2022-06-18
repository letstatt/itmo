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
    solve();
}

struct node {
    char c = '#';
    ll parent = -1;
    ll link = -1;
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
    if (ptr) {
        int link = corasick[corasick[ptr].parent].link;
        while (link != -1 && !corasick[link].m.count(corasick[ptr].c)) {
            link = corasick[link].link;
        }
        corasick[ptr].link = (link == -1 ? 0 : corasick[link].m[corasick[ptr].c]);
    }
}

int go_link(int ptr, char c) {
    while (ptr != -1 && !corasick[ptr].m.count(c)) {
        ptr = corasick[ptr].link;
    }
    return (ptr == -1 ? 0 : corasick[ptr].m[c]);
}

void build() {
    queue<int> q;
    q.push(0);
    while (!q.empty()) {
        add_link(q.front());
        for (auto &i: corasick[q.front()].m) {
            q.push(i.second);
        }
        q.pop();
    }
}

void dfs(int ptr, vector<int>& used) {
    used[ptr] = 2;
    if (corasick[ptr].link != -1 && used[corasick[ptr].link] < 2) {
        dfs(corasick[ptr].link, used);
    }
}

void solve() {
    corasick.reserve(100000);
    corasick.resize(1);

    cin >> m;
    for (int i = 0; i < m; ++i) {
        string s;
        cin >> s;
        add_string(s, i);
    }

    build();
    cin >> t;

    vector<int> used(corasick.size());
    unordered_set<int> ids;
    int ptr = 0;

    for (auto &c: t) {
        ptr = go_link(ptr, c);
        used[ptr] = 1;
    }

    for (int i = 0; i < used.size(); ++i) {
        if (used[i] == 1) {
            dfs(i, used);
        }
    }

    for (int i = 1; i < used.size(); ++i) {
        if (corasick[i].terminal && used[i] == 2) {
            ids.insert(corasick[i].ids.begin(), corasick[i].ids.end());
        }
    }

    for (int i = 0; i < m; ++i) {
        cout << (ids.count(i) ? "YES" : "NO") nl;
    }
}
