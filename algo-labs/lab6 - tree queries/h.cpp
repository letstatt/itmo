#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <climits>
#include <random>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef unsigned long long ull;
typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
#ifndef LOCAL
    //freopen("input.txt", "r", stdin);
    //freopen("output.txt", "w", stdout);
#endif
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct Node {
    int a, b;
    int size = 1;
    ull y;
    Node *p = nullptr, *left = nullptr, *right = nullptr;
    Node(int a, int b, ull y) : a(a), b(b), y(y) {}
};

int n, m;
vector<Node*> v;
vector<map<int, Node*>> g;

int size(Node* t) {
    return t ? t->size : 0;
}

void recalc(Node* t) {
    if (!t) return;
    if (t->left) t->left->p = t;
    if (t->right) t->right->p = t;
    t->size = 1 + size(t->left) + size(t->right);
}

int find_order(Node* t) {
    int res = size(t->left);
    while (t->p) {
        if (t->p->left != t) {
            res += size(t->p->left) + 1;
        }
        t = t->p;
    }
    return res;
}

Node* getp(Node* t) {
    if (!t) return nullptr;
    while (t->p) {
        t = t->p;
    }
    return t;
}

Node* merge(Node *a, Node *b) {
    if (!a || !b) return a ? a : b;

    if (a->y > b->y) {
        a->right = merge(a->right, b);
        recalc(a);
        return a;
    } else {
        b->left = merge(a, b->left);
        recalc(b);
        return b;
    }
}

pair<Node*, Node*> implicit_split(Node *t, int index) {
    if (!t) return {nullptr, nullptr};
    t->p = nullptr;

    if (index <= size(t->left)) {
        auto it = implicit_split(t->left, index);
        t->left = it.second;
        recalc(t);
        return {it.first, t};
    } else {
        auto it = implicit_split(t->right, index - size(t->left) - 1);
        t->right = it.first;
        recalc(t);
        return {t, it.second};
    }
}

// true - t is in first subtree, false - in the second one
pair<Node*, Node*> split_by_node(Node *t, bool pad = true) {
    if (!t) return {nullptr, nullptr};
    int index = find_order(t) + int(pad);
    return implicit_split(getp(t), index);
}

void link(int a, int b, ull y1, ull y2) {
    auto it = split_by_node(v[b], false);
    merge(it.second, it.first);

    it = split_by_node(v[a]);
    g[a][b] = new Node(a, b, y1);
    g[b][a] = new Node(b, a, y2);

    Node *x = merge(getp(it.first), g[a][b]);
    x = merge(x, getp(v[b]));
    x = merge(x, g[b][a]);
    merge(x, getp(it.second));
}

void cut(int a, int b) {
    Node* x = g[a][b], *y = g[b][a];
    if (find_order(x) > find_order(y)) swap(x, y), swap(a, b);

    auto it = split_by_node(x, false);
    auto it2 = implicit_split(it.second, 1);
    //delete it2.first;

    auto it3 = split_by_node(y, false);
    auto it4 = implicit_split(it3.second, 1);
    //delete it4.first;

    merge(getp(it.first), getp(it4.second));
    g[a].erase(b);
    g[b].erase(a);
}

bool connected(int a, int b) {
    return getp(v[a]) == getp(v[b]);
}

void solve() {
    cin >> n >> m;
    v.resize(n + 1, nullptr);
    g.resize(n + 1);

    std::mt19937 prng;

    for (int i = 1; i <= n; ++i) {
        v[i] = new Node(i, i, prng());
    }

    string type;

    while (m--) {
        int a, b;
        cin >> type >> a >> b;

        if (type == "connected") {
            cout << (connected(a, b) ? 1 : 0) << endl;

        } else if (type == "link") {
            link(a, b, prng(), prng());

        } else if (type == "cut") {
            cut(a, b);
        }
    }
}