#include <iostream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <string_view>

using namespace std;

struct node {
    size_t parent;
    size_t pos;
    size_t len;
    unordered_map<char, size_t> to;
    unordered_map<char, size_t> pref_link;
};

const size_t MAXN = 1e5;
node nodes[MAXN];
size_t sz = 2;
size_t v_ans = 0;
size_t len_ans = 0;
string s;

void attach(size_t child, size_t parent, char c, size_t child_len) {
	nodes[parent].to[c] = child;
	nodes[child].len = child_len;
	nodes[child].parent = parent;
}

void extend(size_t i) {
    size_t v;
    size_t vlen = s.size() - i;
    size_t old = sz - 1;
    deque<size_t> path;

    for (v = old; !nodes[v].pref_link.count(s[i]); v = nodes[v].parent) {
        vlen -= nodes[v].len;
        path.push_back(v);
    }

    nodes[sz] = {};
    size_t w = nodes[v].pref_link[s[i]];

    if (nodes[w].to.count(s[i + vlen])) {
        size_t u = nodes[w].to[s[i + vlen]];
        nodes[sz].pos = nodes[u].pos - nodes[u].len;
        for (; s[nodes[sz].pos] == s[i + vlen]; nodes[sz].pos += nodes[v].len) {
            v = path.back();
            path.pop_back();
            vlen += nodes[v].len;
        }
        attach(sz, w, s[nodes[u].pos - nodes[u].len], nodes[u].len - (nodes[u].pos - nodes[sz].pos));
		attach(u, sz, s[nodes[sz].pos], nodes[u].pos - nodes[sz].pos);
		w = nodes[v].pref_link[s[i]] = sz++;
    }
    nodes[old].pref_link[s[i]] = sz;
    attach(sz, w, s[i + vlen], s.size() - (i + vlen));
	nodes[sz++].pos = s.size();
}

void dfs(size_t i, size_t len) {
    if (nodes[i].pos != s.size() && len_ans < len) {
        len_ans = len;
        v_ans = i;
    }

    for (auto [c, v]: nodes[i].to) {
        dfs(v, len + nodes[v].len);
    }
}

int main(int argc, char** argv) {
    if (argc == 3) {
        freopen(argv[1], "r", stdin);
        freopen(argv[2], "w", stdout);
    }

    cin >> s;
    s.append(1, '$');

    // init algorithm
    std::unordered_set<char> alphabet;
    for (char c: s) {
        alphabet.insert(c);
    }

    nodes[0] = {};
    nodes[1] = node{0, 0, 1};

    for (char c : alphabet) {
        nodes[0].pref_link[c] = 1;
    }

    // build suffix tree
    for (int i = s.size() - 1; i >= 0; i--) {
        extend(i);
    }

    // find vertex-answer
    dfs(1, 0);

    // check if answer exists
    if (!v_ans) {
        cout << "nan" << endl;
        return 0;
    }

    // print answer
    deque<pair<size_t, size_t>> st;
    string_view view(s.data());
    size_t v = v_ans;
    
    while (v_ans != 1) {
        st.push_front({nodes[v_ans].pos, nodes[v_ans].len});
        v_ans = nodes[v_ans].parent;
    }

    for (auto [pos, len]: st) {
        cout << view.substr(pos - len, len);
    }
    cout << endl;
}